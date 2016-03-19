module Translate(findEscapes, makeIdsUnique) where
import AST
import Lex(Pos)
import qualified Semant as Semant
import qualified Frame as Fr
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Symbol as S
import qualified Control.Monad.State.Lazy as ST
import qualified Tree as Tr
import qualified Data.List as Data.List
import qualified Control.Monad.RWS as RWS
import qualified Data.Monoid as Monoid
import Data.Maybe(fromJust)
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
  fundecs' <- ST.mapM (\ (Fundec (x, FundecF name params result body)) -> do
                         let Just name' = M.lookup name replaceNames
                         params' <- mapM (\(Field name typ) -> do
                                            name' <- S.genSym
                                            return $ Field name' typ)
                                    params
                         let paramReps = M.fromList $ zip (map fieldName params)
                                         (map fieldName params')
                             replacements' = M.union paramReps replaceNames
                         body' <- makeIdsUniqueInExp replacements' body
                         return $ Fundec (x, FundecF name' params' result body'))
              fundecs
  return (fundecs', replaceNames)


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


data VarAccess = VarAccess Fr.Access S.Symbol S.Label -- access, parentFun, parentLabel
                 deriving (Show)


data AccessMap = AccessMap {amVarAccess :: M.Map S.Symbol VarAccess,
                            amFunNameFrames :: M.Map S.Symbol Fr.Frame,
                            amFunLabelFrames :: M.Map S.Label Fr.Frame}
                 deriving (Show)

varAccess v = fromJust . M.lookup v . amVarAccess
funFrameByName name = fromJust . M.lookup name . amFunNameFrames
funFrameByLabel lab = fromJust . M.lookup lab . amFunLabelFrames

emptyAccessMap = AccessMap M.empty M.empty M.empty

accessMapUnion (AccessMap m1 m2 m3) (AccessMap m1' m2' m3') =
    AccessMap (m1 `M.union` m1') (m2 `M.union` m2') (m3 `M.union` m3')

accessMapUnions = foldr accessMapUnion emptyAccessMap

type LocalAllocation = (S.Symbol, Bool) -- Name of var, whether it escapes

buildAccessMap :: Set.Set S.Symbol -> Exp t t1 t2 t3 t4
               -> ST.State S.SymbolTable (Fr.Frame, AccessMap)
buildAccessMap escapes ast = do
  mainFunSym <- S.genSym
  mainFunLabel <- S.genLabel
  (astAcc, astLoc) <- buildAccessMapFromExp (\var -> Set.member var escapes) mainFunLabel ast
  let (localNames, localEscapes) = unzip astLoc
  (frame, accessMap) <- buildFrame mainFunLabel mainFunLabel mainFunSym [] [] localNames localEscapes
  return (frame, accessMapUnion accessMap astAcc)

joinAccessResults (access1, locals1) (access2, locals2) =
    (accessMapUnion access1 access2, locals1 ++ locals2)

buildAccessMapFromExp :: (S.Symbol -> Bool) -> S.Label -> Exp t t1 t2 t3 t4
                      -> ST.State S.SymbolTable (AccessMap, [LocalAllocation])
buildAccessMapFromExp escapes parent ast@(Exp (x, exp)) =
    let ret x y = return (x, y)
    in case exp of
         ForExp var lo hi body -> do
                  let allocVar = (var, escapes var)
                  (loAcc, loLoc) <- buildAccessMapFromExp escapes parent lo
                  (hiAcc, hiLoc) <- buildAccessMapFromExp escapes parent hi
                  (bodyAcc, bodyLoc) <- buildAccessMapFromExp escapes parent body
                  ret (accessMapUnions [loAcc, hiAcc, bodyAcc])
                      (allocVar : (loLoc ++ hiLoc ++ bodyLoc))
         LetExp decs body -> do
                  (decAcc, decLocals) <- buildAccessMapFromDecs escapes parent decs
                  (bodyAcc, bodyLocals) <- buildAccessMapFromExp escapes parent body
                  ret (accessMapUnion decAcc bodyAcc) (decLocals ++ bodyLocals)
         _ -> foldMExp (buildAccessMapFromExp escapes parent) joinAccessResults (emptyAccessMap, []) ast

buildAccessMapFromDecs :: (S.Symbol -> Bool) -> S.Label -> [Dec t t1 t2 t3 t4]
                       -> ST.State S.SymbolTable (AccessMap, [LocalAllocation])
buildAccessMapFromDecs escapes parent decs = do
  (maps, locals) <- liftM unzip $ mapM (buildAccessMapFromDec escapes parent) decs
  return (accessMapUnions maps, concat locals)

buildAccessMapFromDec :: (S.Symbol -> Bool) -> S.Label -> Dec t t1 t2 t3 t4
                       -> ST.State S.SymbolTable (AccessMap, [LocalAllocation])
buildAccessMapFromDec escapes parent (Dec (x, dec)) =
    case dec of
      FunDec fundecs -> do
               (maps, locals) <- liftM unzip $ mapM (buildAccessMapFromFundec escapes parent) fundecs
               return (accessMapUnions maps, concat locals)
      VarDec name typ init -> do
               let allocVar = (name, escapes name)
               (initAcc, initLoc) <- buildAccessMapFromExp escapes parent init
               return (initAcc, allocVar : initLoc)
      TypeDec decs -> return (emptyAccessMap, [])

buildAccessMapFromFundec :: (S.Symbol -> Bool) -> S.Label -> Fundec t t1 t2 t3 t4
                         -> ST.State S.SymbolTable (AccessMap, [LocalAllocation])
buildAccessMapFromFundec escapes parent ast@(Fundec (x, FundecF funName params result body)) = do
  funLabel <- S.genLabel
  (bodyAcc, bodyLoc) <- buildAccessMapFromExp escapes funLabel body
  let (paramNames, paramEscs) = unzip $ map (\(Field name _) -> (name, escapes name)) params
      (localNames, localEscapes) = unzip bodyLoc
  (frame, accessMap) <- buildFrame parent funLabel funName paramNames paramEscs localNames localEscapes
  return (accessMapUnion bodyAcc accessMap, [])

buildFrame :: S.Label -> S.Label -> S.Symbol -> [S.Symbol] -> [Bool] -> [S.Symbol] -> [Bool]
           -> ST.State S.SymbolTable (Fr.Frame, AccessMap)
buildFrame parent funLabel funName paramNames paramEscs localNames localEscs = do
  let paramEscs' = True : paramEscs -- Static link is passed as always-escaping param
  (paramAccesses, localAccesses) <- Fr.escapesToAccesses paramEscs' localEscs
  let frame = Fr.newFrame paramAccesses localAccesses funLabel parent
      buildEntries = M.fromList . map (\ (name, acc) -> (name, VarAccess acc funName funLabel))
      paramAccessMap = buildEntries $ zip paramNames (tail paramAccesses)
      localAccessMap = buildEntries $ zip localNames localAccesses
      varAccesses = M.union paramAccessMap localAccessMap
      funNameAM = M.fromList [(funName, frame)]
      funLabelAM = M.fromList [(funLabel, frame)]
      accessMap = AccessMap varAccesses funNameAM funLabelAM
  return (frame, accessMap)

frameStaticLink = head . Fr.frameFormals

{-- BEGIN THE TRANSLATION

    placeholder

--}

liftState :: RWS.Monoid w => ST.State s a -> RWS.RWS r w s a
liftState state = do
  init <- RWS.get
  let (a,s) = ST.runState state init
  RWS.put s
  return a

justs :: [Maybe a] -> [a]
justs [] = []
justs (Nothing : xs) = justs xs
justs (Just x : xs) = x : justs xs

asExp = liftState . Tr.asExp
asStm = liftState . Tr.asStm
translateIntoExp access frame breakTo exp = translateExp access frame breakTo exp >>= asExp
translateVarIntoExp access frame breakTo var = translateVar access frame breakTo var >>= asExp
frag fr = Monoid.Endo ([fr]++)

type TranslateResults = RWS.RWS TransConfig (Monoid.Endo [Fr.Fragment]) S.SymbolTable Tr.Ex
data TransConfig = TransConfig { framePtr :: S.Temp
                               , returnRegister :: S.Temp
                               , mallocLabel :: S.Label
                               , stringEqLabel :: S.Label
                               , initArrayLabel :: S.Label}

translate :: TransConfig -> AccessMap -> Fr.Frame -> Semant.TypedExp
          -> ST.State S.SymbolTable (Tr.Ex, [Fr.Fragment])
translate config access mainFrame ast = do
  symTable <- ST.get
  let (trans, symTable', frags) =
          RWS.runRWS (translateExp access mainFrame Nothing ast) config symTable
  ST.put symTable'
  return (trans, Monoid.appEndo frags [])

--translateExp :: AccessMap -> Fr.Frame -> Semant.TypedExp -> ST.State S.SymbolTable Tr.Ex
translateExp :: AccessMap -> Fr.Frame -> Maybe S.Label -> Semant.TypedExp -> TranslateResults
translateExp access frame breakTo (Exp (datum@(pos, expType), exp)) =
    let recur = translateExp access frame breakTo
        recurIntoExp = translateIntoExp access frame breakTo
    in case exp of
      VarExp var -> translateVar access frame breakTo var
      NilExp -> return $ Tr.Ex $ Tr.Const 0
      IntExp i -> return $ Tr.Ex $ Tr.Const i

      StringExp s -> do
               -- Create a new label for the string and a fragment specifying
               -- that the string is to be defined at that label. Return the label.
               label <- liftState S.genLabel
               RWS.tell $ frag $ Fr.StringFrag label s
               return $ Tr.Ex . Tr.Name $ label

      CallExp fun args -> do
               -- Straightforward mapping to Tr.Call except
               -- the static link is passed explicitly as an argument.
               framePtr <- RWS.asks framePtr
               args' <- mapM recurIntoExp args
               let funFrame = funFrameByName fun access
                   funLabel = Fr.frameName funFrame
                   funParentLabel = Fr.parentFrame funFrame
                   staticLink = createStaticLink access framePtr funParentLabel frame
               return $ Tr.Ex $ Tr.Call (Tr.Name funLabel) (staticLink : args')

      OpExp left@(Exp ((_, compType), _)) op right -> do
               leftTrans <- recurIntoExp left
               rightTrans <- recurIntoExp right
               stringEqFun <- RWS.asks stringEqLabel
               let binopEx op = return . Tr.Ex $ Tr.Binop op leftTrans rightTrans
                   cjumpCx op = return . Tr.Cx $ Tr.CJump op leftTrans rightTrans
               case op of
                 {-- Arithmetic is straightfoward --}
                 PlusOp   -> binopEx Tr.Plus
                 MinusOp  -> binopEx Tr.Minus
                 TimesOp  -> binopEx Tr.Mul
                 DivideOp -> binopEx Tr.Div
                 {-- > < >= <= get translated with the Cx representation --}
                 GeOp     -> cjumpCx Tr.Ge
                 GtOp     -> cjumpCx Tr.Gt
                 LtOp     -> cjumpCx Tr.Lt
                 LeOp     -> cjumpCx Tr.Le
                 {-- short-circuiting & and | are desugared to if statements --}
                 AndOp    -> let ifStm = Exp (datum, IfExp left right (Just (Exp (datum, IntExp 0))))
                             in recur ifStm
                 OrOp     -> let ifStm = Exp (datum, IfExp left (Exp (datum, IntExp 1)) (Just right))
                             in recur ifStm
                 {-- Strings are (un)equal if their contents are (un)equal. Array and record
                 equality is pointer equality. --}
                 EqOp     ->
                     case compType of
                       Semant.StrType ->
                           return $ Fr.externalCall stringEqFun [leftTrans, rightTrans]
                       _ -> cjumpCx Tr.Eq
                 NeqOp    ->
                     case compType of
                       Semant.StrType ->
                           let desugar = Exp (datum, IfExp (Exp (datum, OpExp left EqOp right))
                                                       (Exp (datum, IntExp 0))
                                                       (Just (Exp (datum, IntExp 1))))
                           in recur desugar
                       _ -> cjumpCx Tr.Ne

      RecordExp fields _ -> do
               {-- To create {a = x, b = y, c = z}, call malloc(3*wordSize) and store the result
                 in a new temporary t. Then, call MOVE(MEM t, x), MOVE(MEM t+wordSize, y),
                 MOVE(MEM t+2*wordSize, z) --}
               let (_, initExps) = unzip fields
               transInitExps <- mapM recurIntoExp initExps
               malloc <- RWS.asks mallocLabel
               callMalloc <- asExp $ Fr.externalCall malloc [Tr.Const $ Fr.wordSize*(length fields)]
               temp <- liftM Tr.Temp $ liftState S.genTemp
               let createInitStatements initExps fieldNum =
                       case initExps of
                         [] -> Tr.ExpStm $ Tr.Const 0
                         (exp:exps) -> let field = Tr.Mem
                                                   $ Tr.Binop Tr.Plus temp
                                                   $ Tr.Binop Tr.Mul
                                                         (Tr.Const Fr.wordSize)
                                                         (Tr.Const fieldNum)
                                           moveStm = Tr.Move field exp
                                           restInits = createInitStatements exps (fieldNum + 1)
                                       in Tr.Seq moveStm restInits
                   allocStm = Tr.Move temp callMalloc
                   inits = createInitStatements transInitExps 0
                   allocAndInit = Tr.Seq allocStm inits
               return $ Tr.Ex $ Tr.Eseq allocAndInit temp

      ArrayExp _ size init -> do
               {-- translate size and init. Call a runtime function. --}
               transSize <- recurIntoExp size
               transInit <- recurIntoExp init
               initArray <- RWS.asks initArrayLabel
               return $ Fr.externalCall initArray [transSize, transInit]

      SeqExp exps -> do
               -- If there are n exps, n > 0, then execute the first n-1 exps, ignoring
               -- their results, then execute the nth exp and return its result.
               -- If there are 0 exps, return 0.
               exps' <- mapM recurIntoExp exps
               return $ Tr.Ex . intoESeqExp $ exps'
          where intoESeqExp exps =
                    case exps of
                      [] -> Tr.Const 0
                      [x] -> x
                      (x:xs) -> Tr.Eseq (Tr.ExpStm x) (intoESeqExp xs)

      AssignExp lvalue rvalue -> do
               transL <- translateVarIntoExp access frame breakTo lvalue
               transR <- recurIntoExp rvalue
               return $ Tr.Nx $ Tr.Move transL transR

      WhileExp testExp bodyExp -> do
               {-- test : if not(testExp) then goto body else goto done
                 body : bodyExp
                 done : <nothing> --}
               -- Look at that perfect '<-' alignment. I didn't even have to use
               -- M-x align-regexp. It is hard not to see this as some sort of sign...
               -- but what does it augur?
               testLabel <- liftState S.genLabel
               bodyLabel <- liftState S.genLabel
               doneLabel <- liftState S.genLabel
               transTest <- recurIntoExp testExp
               transBody <- translateExp access frame (Just doneLabel) bodyExp >>= asStm
               let test = Tr.Seq (Tr.Label testLabel)
                          $ Tr.CJump Tr.Lt transTest (Tr.Const 1) doneLabel bodyLabel
                   body = Tr.Seq (Tr.Label bodyLabel) transBody
                   done = Tr.Label doneLabel
               return $ Tr.Nx $ Tr.Seq test (Tr.Seq body done)

      ForExp var lo hi body -> do
          {--
            for i := lo to hi body -->
            let var i := lo
                var limit := hi
            in if i < limit
               then while 1 do (body; if i < limit then i := i + 1 else break)

            This works when hi = maxInt, unlike a more straightforward translation into a while loop.
          --}
          limitSym <- liftState S.genSym
          let declareVar = Dec (pos, VarDec var Nothing lo)
              declareLimit = Dec (pos, VarDec limitSym Nothing hi)
              decs = [declareVar, declareLimit]
              varAsVar = Var ((pos, Semant.IntType), SimpleVar var)
              limitAsVar = Var ((pos, Semant.IntType), SimpleVar limitSym)
              varExp v@(Var ((pos, typ), _)) = Exp ((pos, typ), VarExp v)
              -- i < limit
              isVarLessThanLimit = Exp ((pos, Semant.IntType),
                                           OpExp (varExp varAsVar) LtOp (varExp limitAsVar))
              -- i := i + 1
              incVar = Exp (datum, AssignExp varAsVar
                                     (Exp ((pos, Semant.IntType),
                                           OpExp (varExp varAsVar) PlusOp
                                                     (Exp ((pos, Semant.IntType), IntExp 1)))))
              -- if i < limit then i := i + 1 else break
              reenterLoop = Exp (datum, IfExp isVarLessThanLimit incVar
                                          (Just (Exp (datum, BreakExp))))

              -- (body; if i < limit then i := i + 1 else break)
              whileBody = Exp (datum, SeqExp [body, reenterLoop])
              -- while 1 do (body; if i < limit then i := i + 1 else break)
              whileLoop = Exp (datum, WhileExp (Exp ((pos, Semant.IntType), IntExp 1)) whileBody)
              -- if i < limit then while 1 do (body; if i < limit then i := i + 1 else break)
              enterLoop = Exp (datum, IfExp isVarLessThanLimit whileLoop Nothing)
              entireExp = Exp (datum, LetExp decs enterLoop)
          recur entireExp

      BreakExp -> let (Just breakToLabel) = breakTo -- Semant verifies every break is inside loop
                  in return $ Tr.Nx $ Tr.Jump (Tr.Name breakToLabel) [breakToLabel]

      IfExp pred conseq alt -> do
               tpred       <- liftM Tr.asCx $ recur pred
               tconseq     <- recur pred
               talt        <- maybe (return . Tr.Nx $ Tr.ExpStm $ Tr.Const 0) recur alt
               conseqLabel <- liftState S.genLabel
               altLabel    <- liftState S.genLabel
               joinLabel   <- liftState S.genLabel
               result      <- liftM Tr.Temp $ liftState S.genTemp
               let jumpToJoinLabel = Tr.Jump (Tr.Name joinLabel) [joinLabel]
                   unpackage ex =
                       case ex of
                         Tr.Ex exp -> return $ Tr.Seq (Tr.Move result exp) jumpToJoinLabel
                         Tr.Cx genstm -> do
                                 tLabel <- liftState S.genLabel
                                 fLabel <- liftState S.genLabel
                                 let whenTrueDo = Tr.Seq (Tr.Move result (Tr.Const 1))
                                                  jumpToJoinLabel
                                     whenFalseDo = Tr.Seq (Tr.Move result (Tr.Const 0))
                                                   jumpToJoinLabel
                                 return $ Tr.Seq (genstm tLabel fLabel)
                                            $ Tr.Seq (Tr.Label tLabel)
                                            $ Tr.Seq whenTrueDo
                                            $ Tr.Seq (Tr.Label fLabel) whenFalseDo
                         Tr.Nx stm -> return
                                      $ Tr.Seq stm
                                      $ Tr.Seq (Tr.Move result (Tr.Const 0)) jumpToJoinLabel
               conseqBranch <- unpackage tconseq
               altBranch <- unpackage talt
               let branchEtc = Tr.Seq (tpred conseqLabel altLabel)
                               $ Tr.Seq (Tr.Label conseqLabel)
                               $ Tr.Seq conseqBranch
                               $ Tr.Seq (Tr.Label altLabel) altBranch
                   join = Tr.Eseq (Tr.Label joinLabel) result
               return $ Tr.Ex $ Tr.Eseq branchEtc join

      LetExp decs body -> do
                  varInits <- liftM (Tr.seqStm . justs)
                              $ mapM (translateDec access frame breakTo) decs
                  transBody <- recurIntoExp body
                  return $ Tr.Ex $ Tr.Eseq varInits transBody



translateDec :: AccessMap -> Fr.Frame -> Maybe S.Label -> Semant.TypedDec
             -> RWS.RWS TransConfig (Monoid.Endo [Fr.Fragment]) S.SymbolTable (Maybe Tr.Stm)
translateDec access frame breakTo (Dec (_, dec)) =
    case dec of
      TypeDec _ -> return Nothing
      FunDec decs -> mapM_ (translateFunDec access breakTo) decs >> return Nothing
      VarDec name _ init -> do
               transInit <- translateExp access frame breakTo init >>= asExp
               framePtr <- RWS.asks framePtr
               let (VarAccess location _ _ )= varAccess name access
               case location of
                 Fr.InReg temp -> return . Just $ Tr.Move (Tr.Temp temp) transInit
                 -- Local variables are guaranteed to be in this frame; no need to chase
                 -- static links.
                 Fr.InFrame offset ->
                     let varLoc = Tr.Mem $ Tr.Binop Tr.Plus (Tr.Temp framePtr) (Tr.Const offset)
                     in return . Just $ Tr.Move varLoc transInit

translateFunDec access breakTo (Fundec (_, FundecF name params _ body)) = do
  let frame = funFrameByName name access
  returnReg <- liftM Tr.Temp $ RWS.asks returnRegister
  transBody <- translateExp access frame breakTo body >>= asExp
  let bodyWithReturn = Tr.Move returnReg transBody
      bodyWithViewShift = Fr.viewShift frame bodyWithReturn
  RWS.tell $ frag $ Fr.ProcFrag bodyWithViewShift frame
  return ()

translateVar :: AccessMap -> Fr.Frame -> Maybe S.Label -> Semant.TypedVar -> TranslateResults
translateVar access frame breakTo (Var ((_, varType), var)) =
    case var of
      SimpleVar sym ->
          let VarAccess acc varParent varParLabel = varAccess sym access
          in case acc of
               -- If a simple var doesn't escape, it's in some temp register.
               Fr.InReg reg -> return $ Tr.Ex . Tr.Temp $ reg
               -- Otherwise, it's stored in memory, and we need to chase static links to find it.
               Fr.InFrame offset -> do
                        framePtr <- RWS.asks framePtr
                        let fp = createStaticLink access framePtr varParLabel frame
                        return $ Tr.Ex . Tr.Mem $ Tr.Binop Tr.Plus (Tr.Const offset) fp
      FieldVar ofVar sym -> do
               -- a.f is at Mem(+(Mem a [1], wordSize * offset(f) [2])) since
               -- [1] all records are stored on the heap and
               -- [2] all values are one word size large
               ofVarTrans <- translateVar access frame breakTo ofVar >>= asExp
               let Var ((_, (Semant.RecordType fields _)), _) = ofVar
                   Just offset = Data.List.elemIndex sym $ map fst fields
               return $ Tr.Ex $ Tr.Mem
                          $ Tr.Binop Tr.Plus (Tr.Mem ofVarTrans)
                          $ Tr.Const (Fr.wordSize * offset)

      SubscriptVar ofVar subExp -> do
               ofVarTrans <- translateVar access frame breakTo ofVar >>= asExp
               subExpTrans <- translateIntoExp access frame breakTo subExp
               -- for a[i], a is a pointer to the base address of an array.
               -- Therefore a[i] = Mem(+(Mem a, *(i, Const wordSize)))
               return $ Tr.Ex $ Tr.Mem
                          $ Tr.Binop Tr.Plus (Tr.Mem ofVarTrans)
                          $ Tr.Binop Tr.Mul subExpTrans (Tr.Const Fr.wordSize)

createStaticLink access framePtr rootFrame leafFrame =
{-- The root frame's FP is located at
    Mem(+(Const kn, Mem(+(Const kn-1, ...
                            Mem(+(Const k1, Temp FramePointer))...))))
    where each ki is the static link offset in the nested functions.
    Note that Mem(+(Const k1, Temp FramePointer)) is the address of the
    immediate parent frame. --}
  let framesFromRootToLeaf = reverse $ framesFromLeafToRoot rootFrame leafFrame
      framePointer = foldr (\ frame link ->
                                let Fr.InFrame slOffset = frameStaticLink frame
                                in Tr.Mem $ Tr.Binop Tr.Plus (Tr.Const slOffset) link)
                     (Tr.Temp framePtr)
                     framesFromRootToLeaf
  in framePointer
      where framesFromLeafToRoot root leaf =
                if (Fr.frameName leaf) == root
                then []
                else let parentFrame = funFrameByLabel (Fr.parentFrame leafFrame) access
                     in leaf : framesFromLeafToRoot root parentFrame
