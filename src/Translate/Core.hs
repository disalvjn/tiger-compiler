module Translate.Core(TransConfig(..), translate) where
import AST.Core
import AST.Traversal
import Translate.Desugar(desugar)
import Translate.UniqueIds(makeIdsUnique)
import Translate.Access
import Util
import FrontEnd.Lex(Pos)
import qualified Translate.Frame as Fr
import qualified Semant.Core as Semant
import qualified Semant.Type as Type
import qualified Data.Map as M
import qualified Symbol as S
import qualified Control.Monad.State.Strict as ST
import qualified Translate.Tree as Tr
import Translate.Tree( (->-), (->+))
import qualified Data.List as Data.List
import qualified Control.Monad.RWS as RWS
import qualified Data.Monoid as Monoid
import Data.Maybe(catMaybes, fromJust)
import Control.Monad(liftM)

{-- BEGIN THE TRANSLATION

    placeholder

--}

translate :: TransConfig -> Semant.TypedExp
          -> ST.State S.SymbolTable (Fr.Frame, Tr.Stm, [Fr.Fragment])
translate config ast = do
  desugaredAst <- desugar ast
  astWithUniqueIds <- makeIdsUnique desugaredAst
  (mainFrame, access) <- buildAccessMap astWithUniqueIds
  (trans, frags) <- dropState (translateExp access mainFrame Nothing astWithUniqueIds) config
  transStm <- Tr.asStm trans
  return (mainFrame, transStm, Monoid.appEndo frags [])

asExp = liftState . Tr.asExp
asStm = liftState . Tr.asStm
translateIntoExp access frame breakTo exp = translateExp access frame breakTo exp >>= asExp
translateVarIntoExp access frame breakTo var = translateVar access frame breakTo var >>= asExp
frag fr = Monoid.Endo ([fr]++)

type TranslateResults = RWS.RWS TransConfig (Monoid.Endo [Fr.Fragment]) S.SymbolTable Tr.Ex
data TransConfig = TransConfig { registers :: Fr.Registers S.Temp
                               , framePtr :: S.Temp
                               , returnRegister :: S.Temp
                               , mallocLabel :: S.Label
                               , stringEqLabel :: S.Label
                               , initArrayLabel :: S.Label
                               , runTimeFunctions :: M.Map S.Symbol S.Label
                               }


--translateExp :: AccessMap -> Fr.Frame -> Semant.TypedExp -> ST.State S.SymbolTable Tr.Ex
translateExp :: AccessMap -> Fr.Frame -> Maybe S.Label -> Semant.TypedExp -> TranslateResults
translateExp access frame breakTo (Exp (datum@(pos, expType), exp)) =
    let recur = translateExp access frame breakTo
        recurIntoExp = translateIntoExp access frame breakTo
    in case exp of
      VarExp var -> translateVar access frame breakTo var
      NilExp -> return . Tr.Ex $ Tr.Const 0
      IntExp i -> return . Tr.Ex $ Tr.Const i

      StringExp s -> do
               -- Create a new label for the string and a fragment specifying
               -- that the string is to be defined at that label. Return the label.
               label <- liftState S.genLabel
               RWS.tell . frag $ Fr.StringFrag label s
               return . Tr.Ex . Tr.Name $ label

      CallExp fun args -> do
               -- Straightforward mapping to Tr.Call except
               -- the static link is passed explicitly as an argument.
               framePtr <- RWS.asks framePtr
               args' <- mapM recurIntoExp args
               case funFrameByName fun access of
                 -- The function is user defined :
                 Just funFrame ->
                   let funLabel = Fr.frameName funFrame
                       funParentLabel = Fr.parentFrame funFrame
                       staticLink = createStaticLink access framePtr funParentLabel frame
                   in return . Tr.Ex $ Tr.Call (Tr.Name funLabel) (staticLink : args')

                 -- The function wasn't user defined, and is therefore a runtime fn :
                 Nothing -> do
                            Just funLabel <- RWS.asks (M.lookup fun . runTimeFunctions)
                            return . Tr.Ex $ Tr.Call (Tr.Name funLabel) args'

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
                       Type.StrType ->
                           return $ Fr.externalCall stringEqFun [leftTrans, rightTrans]
                       _ -> cjumpCx Tr.Eq
                 NeqOp    ->
                     case compType of
                       Type.StrType ->
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
                                                   (Tr.Const (Fr.wordSize * fieldNum))
                                           moveStm = Tr.Move field exp
                                           restInits = createInitStatements exps (fieldNum + 1)
                                       in moveStm ->- restInits
                   allocStm = Tr.Move temp callMalloc
                   inits = createInitStatements transInitExps 0
                   allocAndInit = allocStm ->- inits
               return . Tr.Ex $ allocAndInit ->+ temp

      ArrayExp _ size init -> do
               {-- translate size and init. Call a runtime function. --}
               transSize <- recurIntoExp size
               transInit <- recurIntoExp init
               initArray <- RWS.asks initArrayLabel
               return $ Fr.externalCall initArray [transInit, transSize]

      SeqExp exps -> do
               -- If there are n exps, n > 0, then execute the first n-1 exps, ignoring
               -- their results, then execute the nth exp and return its result.
               -- If there are 0 exps, return 0.
               exps' <- mapM recurIntoExp exps
               return . Tr.Ex . intoESeqExp $ exps'
          where intoESeqExp exps =
                    case exps of
                      [] -> Tr.Const 0
                      [x] -> x
                      (x:xs) -> Tr.ExpStm x ->+ intoESeqExp xs

      AssignExp lvalue rvalue -> do
               transL <- translateVarIntoExp access frame breakTo lvalue
               transR <- recurIntoExp rvalue
               return . Tr.Nx $ Tr.Move transL transR

      WhileExp testExp bodyExp -> do
               {-- test : if not(testExp) then goto body else goto done
                 body : bodyExp; goto test
                 done : <nothing> --}
               -- Look at that perfect '<-' alignment. I didn't even have to use
               -- M-x align-regexp. It is hard not to see this as some sort of sign...
               -- but what does it augur?
               testLabel <- liftState S.genLabel
               bodyLabel <- liftState S.genLabel
               doneLabel <- liftState S.genLabel
               transTest <- fmap Tr.asCx $ recur testExp
               transBody <- translateExp access frame (Just doneLabel) bodyExp >>= asStm
               let test = Tr.Label testLabel
                          ->- transTest bodyLabel doneLabel
                   body = Tr.Label bodyLabel
                          ->- transBody
                          ->- Tr.Jump (Tr.Name testLabel) [testLabel]
                   done = Tr.Label doneLabel
               return . Tr.Nx $ test ->- body ->- done

      -- ForExp should never happen -- desugar must be run before translation

      BreakExp -> let (Just breakToLabel) = breakTo -- Semant verifies every break is inside loop
                  in return . Tr.Nx $ Tr.Jump (Tr.Name breakToLabel) [breakToLabel]

      IfExp pred conseq alt -> do
               tpred       <- liftM Tr.asCx $ recur pred
               tconseq     <- recur conseq
               talt        <- maybe (return . Tr.Nx . Tr.ExpStm $ Tr.Const 0) recur alt
               conseqLabel <- liftState S.genLabel
               altLabel    <- liftState S.genLabel
               joinLabel   <- liftState S.genLabel
               result      <- liftM Tr.Temp $ liftState S.genTemp
               let jumpToJoinLabel = Tr.Jump (Tr.Name joinLabel) [joinLabel]
                   unpackage ex =
                       case ex of
                         Tr.Ex exp -> return $ Tr.Move result exp ->- jumpToJoinLabel
                         Tr.Cx genstm -> do
                                 tLabel <- liftState S.genLabel
                                 fLabel <- liftState S.genLabel
                                 let whenTrueDo = Tr.Move result (Tr.Const 1)
                                                  ->- jumpToJoinLabel
                                     whenFalseDo = Tr.Move result (Tr.Const 0)
                                                   ->- jumpToJoinLabel
                                 return $ genstm tLabel fLabel
                                      ->- Tr.Label tLabel
                                      ->- whenTrueDo
                                      ->- Tr.Label fLabel
                                      ->- whenFalseDo
                         Tr.Nx stm -> return $ stm
                                      ->- Tr.Move result (Tr.Const 0)
                                      ->- jumpToJoinLabel
               conseqBranch <- unpackage tconseq
               altBranch <- unpackage talt
               let branchEtc = tpred conseqLabel altLabel
                               ->- Tr.Label conseqLabel
                               ->- conseqBranch
                               ->- Tr.Label altLabel
                               ->- altBranch
                   join = Tr.Label joinLabel ->+ result
               return . Tr.Ex $ branchEtc ->+ join

      LetExp decs body -> do
                  varInits <- liftM (Tr.seqStm . catMaybes)
                              $ mapM (translateDec access frame breakTo) decs
                  transBody <- recurIntoExp body
                  return . Tr.Ex $ varInits ->+ transBody


translateDec :: AccessMap -> Fr.Frame -> Maybe S.Label -> Semant.TypedDec
             -> RWS.RWS TransConfig (Monoid.Endo [Fr.Fragment]) S.SymbolTable (Maybe Tr.Stm)
translateDec access frame breakTo (Dec (_, dec)) =
    case dec of
      TypeDec _ -> return Nothing
      FunDec decs -> mapM_ (translateFunDec access breakTo) decs >> return Nothing
      VarDec name _ init -> do
               transInit <- translateExp access frame breakTo init >>= asExp
               framePtr <- RWS.asks framePtr
               let (VarAccess location _ _ ) = varAccess name access
               case location of
                 Fr.InReg temp -> return . Just $ Tr.Move (Tr.Temp temp) transInit
                 -- Local variables are guaranteed to be in this frame; no need to chase
                 -- static links.
                 Fr.InFrame offset ->
                     let varLoc = Tr.Mem $ Tr.Binop Tr.Plus (Tr.Temp framePtr) (Tr.Const offset)
                     in return . Just $ Tr.Move varLoc transInit

translateFunDec access breakTo (Fundec (_, FundecF name params _ body)) = do
  regs <- RWS.asks registers
  -- every declared function has a corresponding frame
  let (Just frame) = funFrameByName name access
      label = Fr.frameName frame
  returnReg <- liftM Tr.Temp $ RWS.asks returnRegister
  transBody <- translateExp access frame breakTo body >>= asExp
  bodyWithViewShiftAndReturn <- liftState . Fr.viewShift regs frame $ Tr.Move returnReg transBody
  RWS.tell . frag $ Fr.ProcFrag (Tr.Label label ->- bodyWithViewShiftAndReturn) frame

translateVar :: AccessMap -> Fr.Frame -> Maybe S.Label -> Semant.TypedVar -> TranslateResults
translateVar access frame breakTo (Var ((_, varType), var)) =
    case var of
      SimpleVar sym ->
          let VarAccess acc varParent varParLabel = varAccess sym access
          in case acc of
               -- If a simple var doesn't escape, it's in some temp register.
               Fr.InReg reg -> return . Tr.Ex . Tr.Temp $ reg
               -- Otherwise, it's stored in memory, and we need to chase static links to find it.
               Fr.InFrame offset -> do
                        framePtr <- RWS.asks framePtr
                        let fp = createStaticLink access framePtr varParLabel frame
                        return . Tr.Ex . Tr.Mem $ Tr.Binop Tr.Plus (Tr.Const offset) fp
      FieldVar ofVar sym -> do
               -- a.f is at Mem(+(a [1], wordSize * offset(f) [2])) since
               -- [1] a is a ptr to the base addr of the record on the heap
               -- [2] all values are one word size large
               ofVarTrans <- translateVar access frame breakTo ofVar >>= asExp
               let Var ((_, (Type.RecordType fields _)), _) = ofVar
                   Just offset = Data.List.elemIndex sym $ map fst fields
               return . Tr.Ex . Tr.Mem
                          $ Tr.Binop Tr.Plus ofVarTrans -- (Tr.Mem ofVarTrans)
                          $ Tr.Const (Fr.wordSize * offset)

      SubscriptVar ofVar subExp -> do
               ofVarTrans <- translateVar access frame breakTo ofVar >>= asExp
               subExpTrans <- translateIntoExp access frame breakTo subExp
               -- for a[i], a is a pointer to the base address of an array.
               -- Therefore a[i] = Mem(+(a, *(i, Const wordSize)))
               return . Tr.Ex . Tr.Mem
                          $ Tr.Binop Tr.Plus ofVarTrans
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
