module Translate(findEscapes, makeIdsUnique) where
import AST
import Lex(Pos)
import qualified Semant as Semant
import qualified Frame as Fr
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Symbol as S
import qualified Control.Monad.State.Lazy as ST
import Control.Monad(liftM)

{-- Making IDs Unique

    Precondition: All used variables are defined (run the AST through Semant!)

    Rename variables to prevent shadowing. This allows us to construct sets of var names
or maps with var names as keys without any ambiguity or scope limitations. Consequently,
building a set of escaping variables and building a map from variables to their frames and
access information becomes much easier.

--}

makeIdsUnique :: Exp e v d t f -> ST.State S.SymbolTable (Exp e v d t f)
makeIdsUnique = makeIdsUniqueInExp M.empty

makeIdsUniqueInExp :: M.Map S.Symbol S.Symbol -> Exp e v d t f
                   -> ST.State S.SymbolTable (Exp e v d t f)
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
         ForExp var lo hi body -> do
                  var' <- S.genSym
                  let replacements' = M.insert var var' replacements
                  lo' <- makeIdsUniqueInExp replacements' lo
                  hi' <- makeIdsUniqueInExp replacements' hi
                  body' <- makeIdsUniqueInExp replacements' body
                  ret $ ForExp var' lo' hi' body'
         LetExp decs body -> do
                  (decs', replacements') <- makeIdsUniqueInDecs replacements decs
                  body' <- makeIdsUniqueInExp replacements' body
                  ret $ LetExp decs' body'
         _ -> mapMExp (makeIdsUniqueInExp replacements) ast

makeIdsUniqueInVar :: M.Map S.Symbol S.Symbol -> Var e v d t f
                   -> ST.State S.SymbolTable (Var e v d t f)
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
      VarDec name typ init -> do
               name' <- S.genSym
               let replacements' = M.insert name name' replacements
               init' <- makeIdsUniqueInExp replacements' init
               return ((Dec (x, VarDec name' typ init')), replacements')
      _ -> return (ast, replacements)

makeIdsUniqueInFundecs replacements fundecs = do
  nameReplacements <- mapM (\(Fundec (x, FundecF name _ _ _)) -> do
                              name' <- S.genSym
                              return (name, name')) fundecs
  let replaceNames = M.union (M.fromList nameReplacements) replacements
  (rfundecs', replacements') <- ST.foldM (\(decs, replacements)
                                      (Fundec (x, FundecF name params result body)) -> do
                                       let (Just name') = M.lookup name replacements
                                       params' <- mapM (\(Field name typ) -> do
                                                          name' <- S.genSym
                                                          return $ Field name' typ)
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


{-- Finding Escaped Vars

    Precondition: each symbol has been made unique by makeIdsUnique.

    Returns a set of all vars that escape. A variable escapes if it must be stored in memory.
In Tiger, this occurs only when a variable declared in a parent function is used in a child function
(and therefore must be accessed by chasing static links).

--}

findEscapes :: Exp e v d t f -> Set.Set S.Symbol
findEscapes = findEscapesInExp Set.empty

findEscapesInExp :: Set.Set S.Symbol -> Exp e v d t f -> Set.Set S.Symbol
findEscapesInExp thisLevel ast@(Exp (_, exp)) =
    case exp of
      VarExp var -> findEscapesInVar thisLevel var
      ForExp var lo hi body ->
          let thisLevel' = Set.insert var thisLevel
              loEsc = findEscapesInExp thisLevel' lo
              hiEsc = findEscapesInExp thisLevel' hi
              bEsc = findEscapesInExp thisLevel' body
          in loEsc `Set.union` hiEsc `Set.union` bEsc
      LetExp decs body -> findEscapesInDecs thisLevel decs body
      _ -> foldExp (findEscapesInExp thisLevel) Set.union Set.empty ast

findEscapesInVar :: Set.Set S.Symbol -> Var e v d t f -> Set.Set S.Symbol
findEscapesInVar thisLevel (Var (_, var)) =
  let symToEsc sym = if sym `Set.member` thisLevel then Set.empty else Set.fromList [sym]
  in case var of
       SimpleVar sym -> symToEsc sym
       FieldVar ofVar sym ->
           let vEsc = findEscapesInVar thisLevel ofVar
               symEsc = symToEsc sym
           in Set.union symEsc vEsc
       SubscriptVar ofVar exp ->
           let vEsc = findEscapesInVar thisLevel ofVar
               eEsc = findEscapesInExp thisLevel exp
           in Set.union vEsc eEsc

findEscapesInDecs :: Set.Set S.Symbol -> [Dec e v d t f] -> Exp e v d t f -> Set.Set S.Symbol
findEscapesInDecs thisLevel decs body =
  let (newLevel, dEscs) = foldr (\ dec (level, escapes) ->
                                         let (level', escapes') = findEscapesInDec level dec
                                         in (level' `Set.union` level,
                                             escapes' `Set.union` escapes))
                                 (thisLevel, Set.empty)
                                 decs
      bEscs = findEscapesInExp newLevel body
  in bEscs `Set.union` dEscs

findEscapesInDec :: Set.Set S.Symbol -> Dec e v d t f -> (Set.Set S.Symbol, Set.Set S.Symbol)
findEscapesInDec thisLevel (Dec (x, dec)) =
    case dec of
      TypeDec ty -> (thisLevel, Set.empty)
      VarDec name typ init ->
          let escapes = findEscapesInExp thisLevel init
          in (Set.insert name thisLevel, escapes)
      FunDec fundecs ->
          let escapes = Set.unions $ map findEscapesInFunDec fundecs
          in (thisLevel, escapes)

findEscapesInFunDec (Fundec (_, FundecF _ params _ body)) =
    let newLevel = Set.fromList $ map fieldName params
    in findEscapesInExp newLevel body

{-- Building the access map

   Precondition: vars have been made unique by makeIdsUnique.

   Return an AccessMap.
--}


data VarAccess = VarAccess {varAccess :: Fr.Access, parentFun :: S.Symbol,
                            parentLabel :: S.Label}
               | FunAccess {funFrame :: Fr.Frame, parentFun :: S.Symbol}
               deriving (Show)

type AccessMap = M.Map S.Symbol VarAccess
type LocalAllocation = (S.Symbol, Bool) -- Name of var, whether it escapes

buildAccessMap :: Set.Set S.Symbol -> Exp t t1 t2 t3 t4
               -> ST.State S.SymbolTable (Fr.Frame, AccessMap)
buildAccessMap escapes ast = do
  mainFunSym <- S.genSym
  (astAcc, astLoc) <- buildAccessMapFromExp (\var -> Set.member var escapes) mainFunSym ast
  let (localNames, localEscapes) = unzip astLoc
  (frame, accessMap) <- buildFrame mainFunSym mainFunSym [] [] localNames localEscapes
  return (frame, M.union accessMap astAcc)

joinAccessResults (access1, locals1) (access2, locals2) =
    (M.union access1 access2, locals1 ++ locals2)

buildAccessMapFromExp :: (S.Symbol -> Bool) -> S.Symbol -> Exp t t1 t2 t3 t4
                      -> ST.State S.SymbolTable (AccessMap, [LocalAllocation])
buildAccessMapFromExp escapes parent ast@(Exp (x, exp)) =
    let ret x y = return (x, y)
    in case exp of
         ForExp var lo hi body -> do
                  let allocVar = (var, escapes var)
                  (loAcc, loLoc) <- buildAccessMapFromExp escapes parent lo
                  (hiAcc, hiLoc) <- buildAccessMapFromExp escapes parent hi
                  (bodyAcc, bodyLoc) <- buildAccessMapFromExp escapes parent body
                  ret (M.unions [loAcc, hiAcc, bodyAcc]) (allocVar : (loLoc ++ hiLoc ++ bodyLoc))
         LetExp decs body -> do
                  (decAcc, decLocals) <- buildAccessMapFromDecs escapes parent decs
                  (bodyAcc, bodyLocals) <- buildAccessMapFromExp escapes parent body
                  ret (M.union decAcc bodyAcc) (decLocals ++ bodyLocals)
         _ -> foldMExp (buildAccessMapFromExp escapes parent) joinAccessResults (M.empty, []) ast

buildAccessMapFromDecs :: (S.Symbol -> Bool) -> S.Symbol -> [Dec t t1 t2 t3 t4]
                       -> ST.State S.SymbolTable (AccessMap, [LocalAllocation])
buildAccessMapFromDecs escapes parent decs = do
  (maps, locals) <- liftM unzip $ mapM (buildAccessMapFromDec escapes parent) decs
  return (M.unions maps, concat locals)

buildAccessMapFromDec :: (S.Symbol -> Bool) -> S.Symbol -> Dec t t1 t2 t3 t4
                       -> ST.State S.SymbolTable (AccessMap, [LocalAllocation])
buildAccessMapFromDec escapes parent (Dec (x, dec)) =
    case dec of
      FunDec fundecs -> do
               (maps, locals) <- liftM unzip $ mapM (buildAccessMapFromFundec escapes parent) fundecs
               return (M.unions maps, concat locals)
      VarDec name typ init -> do
               let allocVar = (name, escapes name)
               (initAcc, initLoc) <- buildAccessMapFromExp escapes parent init
               return (initAcc, allocVar : initLoc)
      TypeDec decs -> return (M.empty, [])

buildAccessMapFromFundec :: (S.Symbol -> Bool) -> S.Symbol -> Fundec t t1 t2 t3 t4
                         -> ST.State S.SymbolTable (AccessMap, [LocalAllocation])
buildAccessMapFromFundec escapes parent ast@(Fundec (x, FundecF funName params result body)) = do
  (bodyAcc, bodyLoc) <- buildAccessMapFromExp escapes funName body
  let (paramNames, paramEscs) = unzip $ map (\(Field name _) -> (name, escapes name)) params
      (localNames, localEscapes) = unzip bodyLoc
  (frame, accessMap) <- buildFrame parent funName paramNames paramEscs localNames localEscapes
  return (M.union bodyAcc accessMap, [])

buildFrame :: S.Symbol -> S.Symbol -> [S.Symbol] -> [Bool] -> [S.Symbol] -> [Bool]
           -> ST.State S.SymbolTable (Fr.Frame, AccessMap)
buildFrame parent funName paramNames paramEscs localNames localEscs = do
  funLabel <- S.genLabel
  let paramEscs' = True : paramEscs -- Static link is passed as always-escaping param
  (paramAccesses, localAccesses) <- Fr.escapesToAccesses paramEscs' localEscs
  let frame = Fr.newFrame paramAccesses localAccesses funLabel
      buildEntries = M.fromList . map (\ (name, acc) -> (name, VarAccess acc funName funLabel))
      paramAccessMap = buildEntries $ zip paramNames (tail paramAccesses)
      localAccessMap = buildEntries $ zip localNames localAccesses
      accessMap = M.insert funName (FunAccess frame parent) $
                  M.union paramAccessMap localAccessMap
  return (frame, accessMap)
