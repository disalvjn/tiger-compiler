module Translate(findEscapes, makeIdsUnique) where
import AST
import Lex(Pos)
import qualified Semant as Semant
import qualified Frame as Fr
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Symbol as S
import qualified Control.Monad.State.Lazy as ST

{-- Finding Escaped Vars

--}

findEscapes = fst . findEscapesInExp Set.empty

-- Actually it will work on any Exp, but the signature would be super ugly
findEscapesInExp :: Set.Set S.Symbol -> Semant.TypedExp -> (Semant.TypedExp, Set.Set S.Symbol)
findEscapesInExp thisLevel ast@(Exp (x, exp)) =
    let ret newExp escapes =  (Exp (x, newExp), escapes)
    in case exp of
         VarExp var ->
             let (var', escapes) = findEscapesInVar thisLevel var
             in ret (VarExp var') escapes
         ForExp var lo hi body _ ->
             let thisLevel' = Set.insert var thisLevel
                 (lo', loEsc) = findEscapesInExp thisLevel' lo
                 (hi', hiEsc) = findEscapesInExp thisLevel' hi
                 (body', bEsc) = findEscapesInExp thisLevel' body
                 escapes = loEsc `Set.union` hiEsc `Set.union` bEsc
             in ret (ForExp var lo' hi' body' $ Set.member var escapes) $ Set.delete var escapes
         LetExp decs body ->
             let (decs', body', escapes) = findEscapesInDecs thisLevel decs body
             in ret (LetExp decs' body') escapes
         _ -> transformExp (findEscapesInExp thisLevel) Set.union Set.empty ast

findEscapesInVar :: Set.Set S.Symbol ->  Semant.TypedVar -> (Semant.TypedVar, Set.Set S.Symbol)
findEscapesInVar thisLevel (Var (x, var)) =
  let ret newVar escapes = (Var (x, newVar), escapes)
      symToEsc sym = if sym `Set.member` thisLevel then Set.empty else Set.fromList [sym]
  in case var of
       SimpleVar sym ->
           ret (SimpleVar sym) (symToEsc sym)
       FieldVar ofVar sym ->
           let (ofVar', vEsc) = findEscapesInVar thisLevel ofVar
               symEsc = symToEsc sym
           in ret (FieldVar ofVar' sym) (Set.union vEsc symEsc)
       SubscriptVar ofVar exp ->
           let (ofVar', vEsc) = findEscapesInVar thisLevel ofVar
               (exp', eEsc) = findEscapesInExp thisLevel exp
           in ret (SubscriptVar ofVar' exp') (Set.union vEsc eEsc)

findEscapesInDecs :: Set.Set S.Symbol -> [Semant.TypedDec] -> Semant.TypedExp ->
                     ([Semant.TypedDec], Semant.TypedExp, Set.Set S.Symbol)
findEscapesInDecs thisLevel decs body =
  let (decs', newLevel, dEscs) = foldr (\ dec (decs, level, escapes) ->
                                         let (dec', level', escapes') = processDec level dec
                                         in (dec':decs, level' `Set.union` level,
                                             escapes' `Set.union` escapes))
                                 ([], thisLevel, Set.empty)
                                 decs
      (body', bEscs) = findEscapesInExp newLevel body
      escapes = bEscs `Set.union` dEscs
      (newEscapes, decsWithEscapedVars) =
          foldr (\dec (escs, decs) -> case dec of
                                        Dec (x, VarDec name typ init esc) ->
                                            let newEscs = Set.delete name escs
                                                escape = Set.member name escapes
                                                newDec = Dec (x, VarDec name typ init escape)
                                            in (newEscs, newDec:decs)
                                        other -> (escs, other:decs))
          (escapes, [])
          decs'
  in (decsWithEscapedVars, body', newEscapes)

processDec :: Set.Set S.Symbol -> Semant.TypedDec ->
              (Semant.TypedDec, Set.Set S.Symbol, Set.Set S.Symbol)
processDec thisLevel (Dec (x, dec)) =
    case dec of
      TypeDec ty -> (Dec (x, TypeDec ty), thisLevel, Set.empty)
      VarDec name typ init escape ->
          let (init', escapes) = findEscapesInExp thisLevel init
          in (Dec (x, VarDec name typ init' escape), Set.insert name thisLevel, escapes)
      FunDec fundecs ->
          let (fundecs', escapes) = foldr (\ fundec (decs, escs) ->
                                            let (fundec', esc) = processFunDec fundec
                                            in (fundec' : decs, esc `Set.union` escs))
                                    ([], Set.empty)
                                    fundecs
          in (Dec (x, FunDec fundecs'), thisLevel, escapes)

--processFunDec :: TypedFundec -> (TypedFundec, Set.Set S.Symbol)
processFunDec (Fundec (x, FundecF name params result body)) =
    let newLevel = Set.fromList $ map fieldName params
        (body', escapes) = findEscapesInExp newLevel body
        newParams = map (\(Field name typ _) -> Field name typ (name `Set.member` escapes)) params
        escapesWoParams = escapes `Set.difference` newLevel
    in (Fundec (x, FundecF name newParams result body'), escapesWoParams)

{-- Making IDs Unique

--}

makeIdsUnique = makeIdsUniqueInExp M.empty

makeIdsUniqueInExp replacements ast@(Exp (x, exp)) =
    let ret newExp = return $ Exp (x, newExp)
    in case exp of
         VarExp var -> do
                  var' <- makeIdsUniqueInVar replacements var
                  ret $ VarExp var'
         CallExp f args -> do
                  let (Just f') = M.lookup f replacements
                  args' <- mapM (makeIdsUniqueInExp replacements) args
                  ret $ CallExp f' args'
         AssignExp var init -> do
                  var' <- makeIdsUniqueInVar replacements var
                  init' <- makeIdsUniqueInExp replacements init
                  ret $ AssignExp var' init'
         ForExp var lo hi body esc -> do
                  var' <- S.genSym
                  let replacements' = M.insert var var' replacements
                  lo' <- makeIdsUniqueInExp replacements' lo
                  hi' <- makeIdsUniqueInExp replacements' hi
                  body' <- makeIdsUniqueInExp replacements' body
                  ret $ ForExp var' lo' hi' body' esc
         LetExp decs body -> do
                  (decs', replacements') <- makeIdsUniqueInDecs replacements decs
                  body' <- makeIdsUniqueInExp replacements' body
                  ret $ LetExp decs' body'
         _ -> mapMExp (makeIdsUniqueInExp replacements) ast

makeIdsUniqueInVar replacements (Var (x, var)) =
    let ret newVar = return $ Var (x, newVar)
    in case var of
         SimpleVar sym -> let (Just sym') = M.lookup sym replacements
                          in ret $ SimpleVar sym'
         FieldVar var sym -> do
                  let (Just sym') = M.lookup sym replacements
                  var' <- makeIdsUniqueInVar replacements var
                  ret $ FieldVar var' sym'
         SubscriptVar var exp -> do
                  var' <- makeIdsUniqueInVar replacements var
                  exp' <- makeIdsUniqueInExp replacements exp
                  ret $ SubscriptVar var' exp'

makeIdsUniqueInDecs replacements decs = do
    (decs', replacements') <- ST.foldM (\(decs, replacements) dec -> do
                                        (dec', replacements') <- makeIdsUniqueInDec replacements dec
                                        return (dec' : decs, replacements'))
                              ([], replacements)
                              decs
    return (reverse decs', replacements')

makeIdsUniqueInDec replacements ast@(Dec (x, dec)) =
    case dec of
      FunDec decs -> do
               (decs', replacements') <- makeIdsUniqueInFundecs replacements decs
               return ((Dec (x, FunDec decs')), replacements')
      VarDec name typ init esc -> do
               name' <- S.genSym
               let replacements' = M.insert name name' replacements
               init' <- makeIdsUniqueInExp replacements' init
               return ((Dec (x, VarDec name' typ init' esc)), replacements')
      _ -> return (ast, replacements)

makeIdsUniqueInFundecs replacements fundecs = do
  nameReplacements <- mapM (\(Fundec (x, FundecF name _ _ _)) -> do
                              name' <- S.genSym
                              return (name, name')) fundecs
  let replaceNames = M.union (M.fromList nameReplacements) replacements
  (rfundecs', replacements') <- ST.foldM (\(decs, replacements)
                                      (Fundec (x, FundecF name params result body)) -> do
                                       let (Just name') = M.lookup name replacements
                                       params' <- mapM (\(Field name typ esc) -> do
                                                          name' <- S.genSym
                                                          return $ Field name' typ esc)
                                                  params
                                       let paramReps = M.fromList $ zip (map fieldName params)
                                                                        (map fieldName params')
                                           replacements' = M.union paramReps replacements
                                       body' <- makeIdsUniqueInExp replacements' body
                                       let newDec = Fundec (x, FundecF name' params' result body')
                                       return $ (newDec : decs, replacements'))
                                ([], replaceNames)
                                fundecs
  let fundecs' = reverse rfundecs'
  return (fundecs', replacements')

{-- Building the access map

--}


data VarAccess = VarAccess {varAccess :: Fr.Access, parentFun :: S.Symbol,
                            parentLabel :: S.Label}
               | FunAccess {funFrame :: Fr.Frame, parentFun :: S.Symbol}
               deriving (Show)

type AccessMap = M.Map S.Symbol VarAccess
type LocalAllocation = (S.Symbol, Bool) -- Name of var, whether it escapes

buildAccessMap :: Exp t t1 t2 t3 t4 -> ST.State S.SymbolTable (Fr.Frame, AccessMap)
buildAccessMap ast = do
  mainFunSym <- S.genSym
  (_, (astAcc, astLoc)) <- buildAccessMapFromExp mainFunSym ast
  let (localNames, localEscapes) = unzip astLoc
  (frame, accessMap) <- buildFrame mainFunSym mainFunSym [] [] localNames localEscapes
  return (frame, M.union accessMap astAcc)

joinAccessResults (access1, locals1) (access2, locals2) =
    (M.union access1 access2, locals1 ++ locals2)

buildAccessMapFromExp :: S.Symbol -> Exp t t1 t2 t3 t4
                      -> ST.State S.SymbolTable (Exp t t1 t2 t3 t4, (AccessMap, [LocalAllocation]))
buildAccessMapFromExp parent ast@(Exp (x, exp)) =
    let ret accessMap locals = return (ast, (accessMap, locals))
    in case exp of
         ForExp var lo hi body escape -> do
                  let allocVar = (var, escape)
                  (_, (loAcc, loLoc)) <- buildAccessMapFromExp parent lo
                  (_, (hiAcc, hiLoc)) <- buildAccessMapFromExp parent hi
                  (_, (bodyAcc, bodyLoc)) <- buildAccessMapFromExp parent body
                  ret (M.unions [loAcc, hiAcc, bodyAcc]) (allocVar : (loLoc ++ hiLoc ++ bodyLoc))
         LetExp decs body -> do
                  (_, (decAcc, decLocals)) <- buildAccessMapFromDecs parent decs
                  (_, (bodyAcc, bodyLocals)) <- buildAccessMapFromExp parent body
                  ret (M.union decAcc bodyAcc) (decLocals ++ bodyLocals)
         _ -> transformMExp (buildAccessMapFromExp parent) joinAccessResults
              (M.empty, []) ast

extractResults = fmap (unzip . (map snd))

buildAccessMapFromDecs parent decs = do
  (maps, locals) <- extractResults $ mapM (buildAccessMapFromDec parent) decs
  return (decs, (M.unions maps, concat locals))

buildAccessMapFromDec parent ast@(Dec (x, dec)) =
    case dec of
      FunDec fundecs -> do
               (maps, locals) <- extractResults $ mapM (buildAccessMapFromFundec parent) fundecs
               return (ast, (M.unions maps, concat locals))
      VarDec name typ init esc -> do
               let allocVar = (name, esc)
               (_, (initAcc, initLoc)) <- buildAccessMapFromExp parent init
               return (ast, (initAcc, allocVar : initLoc))
      TypeDec decs -> return (ast, (M.empty, []))

buildAccessMapFromFundec parent ast@(Fundec (x, FundecF funName params result body)) = do
  (_, (bodyAcc, bodyLoc)) <- buildAccessMapFromExp funName body
  let (paramNames, paramEscs) = unzip $ map (\field -> (fieldName field, fieldEscape field)) params
      (localNames, localEscapes) = unzip bodyLoc
  (frame, accessMap) <- buildFrame parent funName paramNames paramEscs localNames localEscapes
  return (ast, (M.union bodyAcc accessMap, []))

buildFrame parent funName paramNames paramEscs localNames localEscs = do
  funLabel <- S.genLabel
  let paramEscs' = True : paramEscs -- Static link is passed as always-escaping param
  (paramAccesses, localAccesses) <- Fr.escapesToAccesses paramEscs' localEscs
  let frame = Fr.newFrame paramAccesses localAccesses funLabel
      buildEntries = map (\ (name, acc) -> (name, VarAccess acc funName funLabel))
      paramAccessMap = M.fromList . buildEntries $ zip paramNames (tail paramAccesses)
      localAccessMap = M.fromList . buildEntries $ zip localNames localAccesses
      accessMap = M.insert funName (FunAccess frame parent) $
                  M.union paramAccessMap localAccessMap
  return (frame, accessMap)
