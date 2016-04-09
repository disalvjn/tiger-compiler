module Translate.Access(findEscapes, buildAccessMap, AccessMap(..), VarAccess(..),
                       funFrameByName, varAccess, funFrameByLabel, frameStaticLink) where

import AST.Core
import AST.Traversal
import Util
import qualified Translate.Frame as Fr
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Symbol as S
import qualified Control.Monad.State.Strict as ST
import Data.Maybe(fromJust)
import Control.Monad(liftM)

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

buildAccessMap :: Exp t t1 t2 t3 t4 -> ST.State S.SymbolTable (Fr.Frame, AccessMap)
buildAccessMap ast = do
  let escapes = findEscapes ast
  mainFunSym <- S.genSym
  mainFunLabel <- S.genLabel
  (astAcc, astLoc) <- buildAccessMapFromExp (\var -> Set.member var escapes) mainFunLabel ast
  let (localNames, localEscapes) = unzip astLoc
  (frame, accessMap) <- buildFrame mainFunLabel mainFunLabel mainFunSym [] [] localNames localEscapes
  return (frame, accessMapUnion accessMap astAcc)


data VarAccess = VarAccess Fr.Access S.Symbol S.Label -- access, parentFun, parentLabel
                 deriving (Show)


data AccessMap = AccessMap {amVarAccess :: M.Map S.Symbol VarAccess,
                            amFunNameFrames :: M.Map S.Symbol Fr.Frame,
                            amFunLabelFrames :: M.Map S.Label Fr.Frame}
                 deriving (Show)

varAccess v = fromJust . M.lookup v . amVarAccess
funFrameByName name =  M.lookup name . amFunNameFrames
funFrameByLabel lab = fromJust . M.lookup lab . amFunLabelFrames

emptyAccessMap = AccessMap M.empty M.empty M.empty

accessMapUnion (AccessMap m1 m2 m3) (AccessMap m1' m2' m3') =
    AccessMap (m1 `M.union` m1') (m2 `M.union` m2') (m3 `M.union` m3')

accessMapUnions = foldr accessMapUnion emptyAccessMap

type LocalAllocation = (S.Symbol, Bool) -- Name of var, whether it escapes


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
