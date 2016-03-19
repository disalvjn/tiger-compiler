module Translation(tests) where

import Test.HUnit

import qualified Control.Monad.State as ST
import Control.Applicative

import Parse(parse)
import Lex(tokenize)
import AST
import qualified Symbol as S
import qualified Semant as Semant
import qualified Translate as Translate
import qualified Data.Set as Set
import Control.Monad(liftM)
import Debug.Trace(trace)

uniqueIds str =
  let (symTab, tokens) = tokenize str
      ast = parse tokens
  in ST.evalState (Translate.makeIdsUnique ast) symTab

calculateEscapes str =
  let (symTab, tokens) = tokenize str
      ast = parse tokens
  in (Translate.findEscapes ast, symTab)

testcase name = "test/testcases/" ++ name

testMakeUniqueIds = do
  modifiedAST <- liftM uniqueIds $ readFile . testcase $ "testmakeidsunique.tig"
  let Exp (_,LetExp {letDecs = [Dec (_,VarDec {varName = i1, varTyp = _, varInit = _}),
                                 Dec (_ ,FunDec [Fundec (_ ,FundecF {funName = g1, funParams = [Field {fieldName = x1, fieldTyp = _}], funResult = _,
                                                                     funBody = Exp (_,SeqExp [Exp (_,OpExp {opLeft = Exp (_,OpExp {opLeft = Exp (_,VarExp (Var (_,SimpleVar shouldBe'i1'1))),
                                                                                                                                   opOper = PlusOp,
                                                                                                                                   opRight = _}),
                                                                                                            opOper = PlusOp,
                                                                                                            opRight = Exp (_,VarExp (Var (_,SimpleVar shouldBe'x1'1)))}),
                                                                                              Exp (_,LetExp {letDecs = [Dec (_,VarDec {varName = i2, varTyp = _,
                                                                                                                                       varInit = _}),
                                                                                                                        Dec (_,FunDec [Fundec (_,FundecF {funName = g2,
                                                                                                                                                          funParams = [Field {fieldName = x2,
                                                                                                                                                                              fieldTyp = _},
                                                                                                                                                                       Field {fieldName = i3,
                                                                                                                                                                              fieldTyp = _}],
                                                                                                                                                          funResult = _,
                                                                                                                                                          funBody = Exp (_,OpExp {opLeft = Exp (_,VarExp (Var (_,SimpleVar shouldBe'x2'1))),
                                                                                                                                                                                  opOper = PlusOp,
                                                                                                                                                                                  opRight = Exp (_,VarExp (Var (_,SimpleVar shouldBe'i3'1)))})})])],
                                                                                                             letBody = Exp (_,SeqExp [Exp (_,CallExp {callFunc = shouldBe'g2'1,
                                                                                                                                                      callArgs = [Exp (_,OpExp {opLeft = Exp (_,VarExp (Var (_,SimpleVar shouldBe'x1'2))),
                                                                                                                                                                                opOper = PlusOp,
                                                                                                                                                                                opRight = Exp (_,VarExp (Var (_,SimpleVar shouldBe'i2'1)))})]})])})])})]),
                                 Dec (_,VarDec {varName = i4, varTyp = _, varInit = _}),
                                 Dec (_,FunDec [Fundec (_,FundecF {funName = g3, funParams = [Field {fieldName = x3, fieldTyp = _}],
                                                                   funResult = _, funBody = Exp (_,OpExp {opLeft = Exp (_,OpExp {opLeft = Exp (_,VarExp (Var (_,SimpleVar shouldBe'i4'1))),
                                                                                                                                 opOper = PlusOp,
                                                                                                                                 opRight = _}),
                                                                                                          opOper = PlusOp,
                                                                                                          opRight = Exp (_,CallExp {callFunc = shouldBe'f1'1,
                                                                                                                                    callArgs = [Exp (_, VarExp (Var (_,SimpleVar shouldBe'x3'1)))]})})}),
                                                Fundec (_,FundecF {funName = f1, funParams = [Field {fieldName = x4, fieldTyp = _}],
                                                                   funResult = _, funBody = Exp (_, CallExp {callFunc = shouldBe'g3'1,
                                                                                                             callArgs = [Exp (_, VarExp (Var (_,SimpleVar shouldBe'x4'1)))]})})])],
                      letBody = Exp (_,SeqExp [Exp (_,ForExp {forVar = i5, forLo = _, forHi = _,
                                                              forBody = Exp (_,CallExp {callFunc = shouldBe'g3'2,
                                                                                        callArgs = [Exp (_,VarExp (Var (_,SimpleVar shouldBe'i5'1)))]})})])})
        = modifiedAST
  assertBool "g2 == shouldBe'g2'1" $ g2 == shouldBe'g2'1
  assertBool "g3 == shouldBe'g3'1" $ g3 == shouldBe'g3'1
  assertBool "g3 == shouldBe'g3'2" $ g3 == shouldBe'g3'2
  assertBool "f1 == shouldBe'f1'1" $ f1 == shouldBe'f1'1
  assertBool "i1 == shouldBe'i1'1" $ i1 == shouldBe'i1'1
  assertBool "i2 == shouldBe'i2'1" $ i2 == shouldBe'i2'1
  assertBool "i3 == shouldBe'i3'1" $ i3 == shouldBe'i3'1
  assertBool "i4 == shouldBe'i4'1" $ i4 == shouldBe'i4'1
  assertBool "x1 == shouldBe'x1'1" $ x1 == shouldBe'x1'1
  assertBool "x1 == shouldBe'x1'2" $ x1 == shouldBe'x1'2
  assertBool "x2 == shouldBe'x2'1" $ x2 == shouldBe'x2'1
  assertBool "x3 == shouldBe'x3'1" $ x3 == shouldBe'x3'1
  assertBool "x4 == shouldBe'x4'1" $ x4 == shouldBe'x4'1
  assertBool "all the syms are pairwise unequal" $ (Set.size (Set.fromList [g1, g2, g3, f1, i1, i2, i3, i4, x1, x2, x3, x4])) == 12


testFindEscapes = do
  (escapesSet, table) <- liftM calculateEscapes $ readFile . testcase $ "testescapes.tig"
  let Just i = S.symbol "i" table
      Just j = S.symbol "j" table
      Just k = S.symbol "k" table
      Just x = S.symbol "x" table
      Just z = S.symbol "z" table
      Just n = S.symbol "n" table
      Just y = S.symbol "y" table
      Just f = S.symbol "f" table
      Just h = S.symbol "h" table
      Just u = S.symbol "u" table
      escapes = \s -> Set.member s escapesSet
  assertBool "i escapes" $ escapes i
  assertBool "j escapes" $ escapes j
  assertBool "k doesn't escape" $ not . escapes $ k
  assertBool "x doesn't escape" $ not . escapes $ x
  assertBool "z escapes" $ escapes z
  assertBool "n doesn't escape" $ not . escapes $ n
  assertBool "y doesn't escape" $ not . escapes $ y
  assertBool "f escapes" $ escapes f
  assertBool "h doesn't escape" $ not . escapes $ h
  assertBool "u doesn't escape" $ not . escapes $ u


tests = test [testMakeUniqueIds, testFindEscapes]
