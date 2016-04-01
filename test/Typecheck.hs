module Typecheck(tests) where

import Test.HUnit

import qualified Control.Monad.State.Strict as ST
import Control.Applicative

import Parse(parse)
import Lex(tokenize)
import AST
import qualified Semant as Semant
import Debug.Trace(trace)

data ErrorType = UndefVar | UndefType | UndefField | CircularType | WrongType | ExpectedArray
               | ExpectedRecord | ExpectedFunction | NotARecord | WrongArity | MultipleDeclarations
               | EveryoneKnowsFunctionsArentValues | UnconstrainedNil | BreakNotInForWhile
                 deriving (Show, Eq)

errorIsOfType t err =
    case err of
      Semant.UndefVar _ _ -> t == UndefVar
      Semant.UndefType _ _ -> t == UndefType
      Semant.UndefField _ _ -> t == UndefField
      Semant.CircularType _ -> t == CircularType
      Semant.WrongType _ _ _ -> t == WrongType
      Semant.ExpectedArray _ _ -> t == ExpectedArray
      Semant.ExpectedRecord _ _ _ -> t == ExpectedRecord
      Semant.ExpectedFunction _ _ _ -> t == ExpectedFunction
      Semant.NotARecord _ _ -> t == NotARecord
      Semant.WrongArity _ _ _ _ -> t == WrongArity
      Semant.MultipleDeclarations _ _ -> t == MultipleDeclarations
      Semant.EveryoneKnowsFunctionsArentValues _ _ -> t == EveryoneKnowsFunctionsArentValues
      Semant.UnconstrainedNil _ -> t == UnconstrainedNil
      Semant.BreakNotInForWhile _ -> t == BreakNotInForWhile

printTypecheckedAST = False

typecheck str =
  let (symTab, tokens) = tokenize str
      ast = parse tokens
      (env, symTab') = ST.runState Semant.rootEnv symTab
      result = Semant.analyze env ast
  in if printTypecheckedAST
     then (trace (show result) result)
     else result

typecheckTestCase filename =
    typecheck <$> (readFile $ "test/testcases/appel\'s/" ++ filename)

assertError ofType typechecked =
    either (\e -> assertBool ("error " ++ (show e) ++ " is of type " ++ (show ofType))
                  (errorIsOfType ofType e))
               (\v -> assertFailure ("woah a failure! expected error like " ++ (show ofType)
                                     ++ " but actually got: \n" ++ (show v))) typechecked

assertType predMsgs  typechecked =
    case typechecked of
      Left err -> assertFailure (show err)
      Right t -> mapM_ (\(pred, msg) ->  assertBool msg $ pred t) predMsgs

isAnArrayOf elemType (Exp ((_, t), _)) =
    case t of
      Semant.ArrayType inner _ -> inner == elemType
      _ -> False

isARecordOf elemTypes (Exp ((_, t), _)) =
    case t of
      Semant.RecordType fields _ -> and $ zipWith Semant.isSubtypeOf elemTypes (map snd fields)
      _ -> False

isARecursiveRecord (Exp ((_, t), _)) =
    case t of
      Semant.RecordType fields id -> any (\typ -> case typ of
                                                    Semant.NameType _ _ (Just ref) -> ref == id
                                                    Semant.RecordType _ rid -> rid == id
                                                    _ -> False)
                                     (map snd fields)
      _ -> False

isSimply typ (Exp ((_, t), _)) = typ == t

testTypeDecs =
    test ["test1.tig : array type and variable" ~:
           assertType [(isAnArrayOf Semant.IntType, "is an array of ints")]
                          <$> (typecheckTestCase "test1.tig")
         ,
           "test2.tig : type Y can be used interchangeably with type X if type X = Y" ~:
           assertType [(isAnArrayOf Semant.IntType, "myint should be equivalent to int")]
                          <$> (typecheckTestCase "test2.tig")
         ,
           "test3.tig : Assigning to non-recursive records" ~:
           assertType [(isARecordOf [Semant.StrType, Semant.IntType], "record of str, int")]
                          <$> (typecheckTestCase "test3.tig")
         ,
           "test4.tig : Recursive factorial function" ~:
           assertType [(isSimply Semant.IntType, "return type of recursive function is int")]
                          <$> (typecheckTestCase "test4.tig")
         ,
           "test5.tig : mutually recursive types" ~:
           assertType [(isARecordOf [Semant.IntType, Semant.NilType], "int and itself (or nil)"),
                       (isARecursiveRecord, "recursive record")]
                          <$> (typecheckTestCase "test5.tig")
         ,
           "test6.tig : mutually recursive procedures" ~:
           assertType [(isSimply Semant.UnitType, "neither procedure produces a result")]
                          <$> (typecheckTestCase "test6.tig")
         ,
           "test7.tig : mutually recursive functions" ~:
           assertType [(isSimply Semant.IntType, "return value of called function")]
                          <$> (typecheckTestCase "test7.tig")
         ,
           "test16.tig : type cycle" ~:
           assertError CircularType <$> (typecheckTestCase "test16.tig")
         ,
           "test17.tig : definition of recursive types can't be interrupted" ~:
           assertError UndefType <$> (typecheckTestCase "test17.tig")
         ,
           "test18.tig : definition of recursive functions can't be interrupted" ~:
           assertError UndefVar <$> (typecheckTestCase "test18.tig")
         ,
          "test21.tig : procedure should return unit but returns int" ~:
          assertError WrongType <$> (typecheckTestCase "test21.tig")
         ,
          "test22.tig : field not in record type" ~:
          assertError UndefField <$> (typecheckTestCase "test22.tig")
         ,
          "test28.tig : structurually equivalent records are not interchangeable" ~:
          assertError WrongType <$> (typecheckTestCase "test28.tig")
         ,
          "test29.tig : structurually equivalent arrays are not interchangeable" ~:
          assertError WrongType <$> (typecheckTestCase "test29.tig")
         ,
          "test30.tig : type synonyms are fine" ~:
          assertType [(isSimply Semant.IntType, "arr1[n] is elem type")]
                         <$> (typecheckTestCase "test30.tig")
         ,
          "test31.tig : var's type constraint and type of init value differ" ~:
          assertError WrongType <$> (typecheckTestCase "test31.tig")
         ,
          "test32.tig : initializing exp and array type differ" ~:
          assertError WrongType <$> (typecheckTestCase "test32.tig")
         ,
          "test33.tig : undef type" ~:
          assertError UndefType <$> (typecheckTestCase "test33.tig")
         ,
          "test37.tig : variables can be shadowed" ~:
           assertType [(isSimply Semant.IntType, "everything is okay")]
                          <$> (typecheckTestCase "test37.tig")
         ,
          "test38.tig : multiple declarations of type in same block" ~:
          assertError MultipleDeclarations <$> (typecheckTestCase "test38.tig")
         ,
          "test39.tig : multiple declarations of function in same block" ~:
          assertError MultipleDeclarations <$> (typecheckTestCase "test39.tig")
         ,
          "test40.tig : procedure returns value" ~:
          assertError WrongType <$> (typecheckTestCase "test40.tig")
         ,
          "test41.tig : local type shadows global type" ~:
          assertType [(isSimply Semant.IntType, "a is shadowed")]
                         <$> (typecheckTestCase "test41.tig")
         ,
          "test47.tig : types can shadow types in different blocks" ~:
          assertType [(isSimply Semant.IntType, "shadowing happens")]
                         <$> (typecheckTestCase "test47.tig")
         ,
          "test48.tig : functions can shadow functions in different blocks" ~:
          assertType [(isSimply Semant.IntType, "shadowing happens")]
                         <$> (typecheckTestCase "test48.tig")

         ]

testExps =
    test [ "test8.tig : correct if" ~:
           assertType [(isSimply Semant.IntType, "type of each branch")]
                          <$> (typecheckTestCase "test8.tig")
         ,
          "test9.tig : branches of if differ" ~:
          assertError WrongType <$> (typecheckTestCase "test9.tig")
         ,
          "test10.tig : body of while not unit" ~:
          assertError WrongType <$> (typecheckTestCase "test10.tig")
         ,
          "test11.tig : for's hi and lo must be ints" ~:
          assertError WrongType <$> (typecheckTestCase "test11.tig")
         ,
          "test12.tig : valid for and let" ~:
          assertType [(isSimply Semant.UnitType, "for's type is Unit")]
                         <$> (typecheckTestCase "test12.tig")
         ,
           "test13.tig : comparison of incompatible types" ~:
           assertError WrongType <$> (typecheckTestCase "test13.tig")
         ,
           "test14.tig : invalid comparison between rec and array" ~:
           assertError WrongType <$> (typecheckTestCase "test14.tig")
         ,
           "test15.tig : if-then returns non-unit" ~:
           assertError WrongType <$> (typecheckTestCase "test15.tig")
         ,
           "test26.tig : adding int and string" ~:
           assertError WrongType <$> (typecheckTestCase "test26.tig")
         ,
           "test34.tig : in fun call, arg types don't match param types" ~:
           assertError WrongType <$> (typecheckTestCase "test34.tig")
         ,
           "test35.tig : more formal parameters than arguments" ~:
           assertError WrongArity <$> (typecheckTestCase "test35.tig")
         ,
           "test36.tig : more arguments than formal parameters" ~:
           assertError WrongArity <$> (typecheckTestCase "test36.tig")
         ,
           "test42.tig : correct declarations and assignments etc." ~:
           assertType [(isSimply Semant.UnitType, "lots of assignment...")]
                          <$> (typecheckTestCase "test42.tig")
         ,
           "test43.tig : can't add unit and number" ~:
           assertError WrongType <$> (typecheckTestCase "test43.tig")
         ,
           "test44.tig : valid nil initialization and assignment" ~:
           assertType [(isSimply Semant.UnitType, "an assignment")]
                          <$> (typecheckTestCase "test44.tig")
         ,
           "test45.tig : only records can be nil" ~:
           assertError UnconstrainedNil <$> (typecheckTestCase "test45.tig")
         ,
           "test46.tig : records can be compared" ~:
           assertType [(isSimply Semant.IntType, "ints are bools")]
                          <$> (typecheckTestCase "test46.tig")
         ]

testVars =
    test ["test19.tig : undeclared variable in function body" ~:
           assertError UndefVar <$> (typecheckTestCase "test19.tig")
         ,
           "test20.tig : undeclared variable in seqexp" ~:
           assertError UndefVar <$> (typecheckTestCase "test20.tig")
         ,
           "test23.tig : record field assigned wrong type" ~:
           assertError WrongType <$> (typecheckTestCase "test23.tig")
         ,
          "test24.tig : taking subscript of non-array variable" ~:
          assertError ExpectedArray <$> (typecheckTestCase "test24.tig")
         ,
          "test25.tig : non-record treated as record" ~:
          assertError ExpectedRecord <$> (typecheckTestCase "test25.tig")
         ]

testBreakPlacement =
    test ["test51.tig : misplaced break " ~:
          assertError BreakNotInForWhile <$> (typecheckTestCase "test51.tig")
         ,
          "test52.tig : properly placed break " ~:
          assertType [(isSimply Semant.IntType, "everything okay")]
                         <$> (typecheckTestCase "test52.tig")
         ]



tests = test [testTypeDecs, testExps, testVars, testBreakPlacement]
