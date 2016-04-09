module Translate.UniqueIds(makeIdsUnique) where

import AST.Core
import AST.Traversal
import qualified Control.Monad.State.Strict as ST
import qualified Symbol as S
import qualified Data.Map as M

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
                  -- This is sketchy... it argues that if there's an undefined
                  -- function, then it's a runtime function (like "print"), and
                  -- we can just use its own symbol as its replacement.
                  -- I should fix this in the future by passing in a
                  -- a list of runtime functions or something.
                  -- Also need to do something about what frames they're in...
                  let f' = maybe f id (M.lookup f replacements)
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
                  var' <- makeIdsUniqueInVar replacements var
                  ret $ FieldVar var' sym
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
