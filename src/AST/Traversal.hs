module AST.Traversal(mapExp, mapMExp, transformExp, transformMExp, foldExp, foldMExp) where

import Control.Monad(liftM)
import AST.Core

-- Lots of boiler plate... the types speak for themselves.

transformExpF :: (exp -> (exp', a)) -> (var -> (var', a)) -> (dec -> (dec', a))
                 -> (a -> b -> b) -> b -> ExpF exp var dec -> (ExpF exp' var' dec', b)
transformExpF expf varf decf combine nil exp =
  case exp of
    VarExp var             -> let (var', x) = varf var
                              in (VarExp var', x `combine` nil)
    NilExp                 -> (NilExp, nil)
    IntExp i               -> (IntExp i, nil)
    StringExp s            -> (StringExp s, nil)
    CallExp f args         -> let (args', x) = unpackage $ map expf args
                              in (CallExp f args', x)
    OpExp left op right    -> let (left', l) = expf left
                                  (right', r) = expf right
                              in (OpExp left' op right', l `combine` (r `combine` nil))
    RecordExp fields typ   -> let (fieldNames, fieldExps) = unzip fields
                                  (newExps, x) = unpackage $ map expf fieldExps
                              in (RecordExp (zip fieldNames newExps) typ, x)
    SeqExp exps            -> let (exps', x) = unpackage $ map expf exps
                              in (SeqExp exps', x)
    AssignExp lval rval    -> let (lval', x) = varf lval
                                  (rval', y) = expf rval
                              in (AssignExp lval' rval', x `combine` (y `combine` nil))
    IfExp pred conseq alt  -> let (pred', x) = expf pred
                                  (conseq', y) = expf conseq
                                  (alt', z) = maybe
                                              (Nothing, nil)
                                              (\(a, b) -> (Just a, b `combine` nil))
                                              (fmap expf alt)
                              in (IfExp pred' conseq' alt',
                                  x `combine` (y `combine` z))
    WhileExp test body     -> let (test', x) = expf test
                                  (body', y) = expf body
                              in (WhileExp test' body', x `combine` (y `combine` nil))
    ForExp i lo hi body    -> let (lo', x) = expf lo
                                  (hi', y) = expf hi
                                  (body', z) = expf body
                              in (ForExp i lo' hi' body',
                                  x `combine` (y `combine` (z `combine` nil)))
    BreakExp               -> (BreakExp, nil)
    LetExp decs body       -> let (decs', xs) = unzip $ map decf decs
                                  (body', y) = expf body
                              in (LetExp decs' body', foldr combine (y `combine` nil) xs )
    ArrayExp typ size init -> let (size', x) = expf size
                                  (init', y) = expf init
                              in (ArrayExp typ size' init', x `combine` (y `combine` nil))
    where unpackage expsAndVals =
              let (exps, vals) = unzip expsAndVals
                  joinedVals = foldr combine nil vals
              in (exps, joinedVals)

package f x = (f x, ())
packageM f x = f x >>= return . (\y -> (y, ()))
discard _ _ = ()

foldExpF expf varf decf combine nil exp = snd $ transformExpF expf varf decf combine nil exp

mapExpF expf varf decf exp =
    fst $ transformExpF (package expf) (package varf) (package decf) discard () exp

transformVarF :: (exp -> (exp', a)) -> (var -> (var', a)) ->
                 (a -> b -> b) -> b -> VarF exp var -> (VarF exp' var', b)
transformVarF expf varf combine nil var =
    case var of
      SimpleVar s      -> (SimpleVar s, nil)
      FieldVar v s     -> let (v', x) = varf v
                          in (FieldVar v' s, x `combine` nil)
      SubscriptVar v e -> let (v', x) = varf v
                              (e', y) = expf e
                          in (SubscriptVar v' e', x `combine` (y `combine` nil))

foldVarF expf varf combine nil exp = snd $ transformVarF expf varf combine nil exp

mapVarF expf varf exp =
    fst $ transformVarF (package expf) (package varf) discard () exp

transformDecF :: (exp -> (exp', a)) -> (fundec -> (fundec', a)) -> (ty -> (ty', a))
              -> (a -> b -> b) -> b -> DecF exp fundec ty -> (DecF exp' fundec' ty', b)
transformDecF expf fundecf tyf combine nil dec =
    case dec of
      FunDec decs            -> let (decs', xs) = unzip $ map fundecf decs
                                in (FunDec decs', foldr combine nil xs)
      VarDec name typ init   -> let (init', x) = expf init
                                in (VarDec name typ init', x `combine` nil)
      TypeDec decs           -> let (tyNames, tyDecs) = unzip decs
                                    (tyDecs', xs) = unzip $ map tyf tyDecs
                                in (TypeDec (zip tyNames tyDecs'), foldr combine nil xs)

foldDecF expf fundecf tyf combine nil dec = snd $ transformDecF expf fundecf tyf combine nil dec

mapDecF expf fundecf tyf dec =
    fst $ transformDecF (package expf) (package fundecf) (package tyf) discard () dec

instance Functor FundecF where
    fmap expf (FundecF name params result body) = FundecF name params result (expf body)

transformMExpF :: (Monad m) =>
                 (exp -> m (exp', a)) -> (var -> m (var', a)) -> (dec -> m (dec', a))
                 -> (a -> b -> b) -> b -> ExpF exp var dec -> m (ExpF exp' var' dec', b)
transformMExpF expf varf decf combine nil exp =
  case exp of
    VarExp var             -> do (var', x) <- varf var
                                 return (VarExp var', x `combine` nil)
    NilExp                 -> return (NilExp, nil)
    IntExp i               -> return (IntExp i, nil)
    StringExp s            -> return (StringExp s, nil)
    CallExp f args         -> do (args', x) <- liftM unpackage $ mapM expf args
                                 return (CallExp f args', x)
    OpExp left op right    -> do (left', l) <- expf left
                                 (right', r) <- expf right
                                 return (OpExp left' op right', l `combine` (r `combine` nil))
    RecordExp fields typ   -> do let (fieldNames, fieldExps) = unzip fields
                                 (newExps, x) <- liftM unpackage $ mapM expf fieldExps
                                 return (RecordExp (zip fieldNames newExps) typ, x)
    SeqExp exps            -> do (exps', x) <- liftM unpackage $ mapM expf exps
                                 return (SeqExp exps', x)
    AssignExp lval rval    -> do (lval', x) <- varf lval
                                 (rval', y) <- expf rval
                                 return (AssignExp lval' rval', x `combine` (y `combine` nil))
    IfExp pred conseq alt  -> do (pred', x) <- expf pred
                                 (conseq', y) <- expf conseq
                                 (alt', z) <- case alt of
                                                Nothing -> return (Nothing, nil)
                                                Just a -> do
                                                         (a', b) <- expf a
                                                         return (Just a', b `combine` nil)
                                 return (IfExp pred' conseq' alt',
                                         x `combine` (y `combine` z))
    WhileExp test body     -> do (test', x) <- expf test
                                 (body', y) <- expf body
                                 return (WhileExp test' body', x `combine` (y `combine` nil))
    ForExp i lo hi body    -> do (lo', x) <- expf lo
                                 (hi', y) <- expf hi
                                 (body', z) <- expf body
                                 return (ForExp i lo' hi' body',
                                         x `combine` (y `combine` (z `combine` nil)))
    BreakExp               -> return (BreakExp, nil)
    LetExp decs body       -> do (decs', xs) <- liftM unzip $ mapM decf decs
                                 (body', y) <- expf body
                                 return (LetExp decs' body', foldr combine (y `combine` nil) xs )
    ArrayExp typ size init -> do (size', x) <- expf size
                                 (init', y) <- expf init
                                 return (ArrayExp typ size' init', x `combine` (y `combine` nil))
    where unpackage expsAndVals =
              let (exps, vals) = unzip expsAndVals
                  joinedVals = foldr combine nil vals
              in (exps, joinedVals)

mapMExpF expf varf decf exp =
    liftM fst $ transformMExpF (packageM expf) (packageM varf) (packageM decf) discard () exp

foldMExpF expf varf decf combine nil exp =
    liftM snd $ transformMExpF expf varf decf combine nil exp

transformMVarF :: (Monad m) =>
                 (exp -> m (exp', a)) -> (var -> m (var', a)) ->
                 (a -> b -> b) -> b -> VarF exp var -> m (VarF exp' var', b)
transformMVarF expf varf combine nil var =
    case var of
      SimpleVar s      -> return (SimpleVar s, nil)
      FieldVar v s     -> do (v', x) <- varf v
                             return (FieldVar v' s, x `combine` nil)
      SubscriptVar v e -> do (v', x) <- varf v
                             (e', y) <- expf e
                             return (SubscriptVar v' e', x `combine` (y `combine` nil))

mapMVarF expf varf var =
    liftM fst $ transformMVarF (packageM expf) (packageM varf) discard () var

foldMVarF expf varf combine nil var =
    liftM snd $ transformMVarF expf varf combine nil var

transformMDecF :: (Monad m) =>
                  (exp -> m (exp', a)) -> (fundec -> m (fundec', a)) -> (ty -> m (ty', a))
              -> (a -> b -> b) -> b -> DecF exp fundec ty -> m (DecF exp' fundec' ty', b)
transformMDecF expf fundecf tyf combine nil dec =
    case dec of
      FunDec decs            -> do (decs', xs) <- liftM unzip $ mapM fundecf decs
                                   return (FunDec decs', foldr combine nil xs)
      VarDec name typ init   -> do (init', x) <- expf init
                                   return (VarDec name typ init', x `combine` nil)
      TypeDec decs           -> do let (tyNames, tyDecs) = unzip decs
                                   (tyDecs', xs) <- liftM unzip $ mapM tyf tyDecs
                                   return (TypeDec (zip tyNames tyDecs'), foldr combine nil xs)

mapMDecF expf fundecf tyf dec =
    liftM fst $ transformMDecF (packageM expf) (packageM fundecf) (packageM tyf) discard () dec

foldMDecF expf fundecf tyf combine nil dec =
    liftM snd $ transformMDecF expf fundecf tyf combine nil dec

mapMFundecF :: (Monad m) => (exp -> m exp') -> FundecF exp -> m (FundecF exp')
mapMFundecF expf (FundecF name params result body) =
    expf body >>= return . (FundecF name params result)


transformExp f combine nil (Exp (x, e)) =
    let (e', res) = transformExpF f
                    (transformExpsInVar f combine nil)
                    (transformExpsInDec f combine nil)
                    combine nil e
    in (Exp (x, e'), res)

transformExpsInVar f combine nil (Var (x, v)) =
    let (v', res) = transformVarF f (transformExpsInVar f combine nil) combine nil v
    in (Var (x, v'), res)

transformExpsInDec f combine nil (Dec (x, dec)) =
    let (dec', res) = transformDecF f (transformExpsInFundec f combine nil)
                      (\x -> (x, nil)) combine nil dec
    in (Dec (x, dec'), res)

transformExpsInFundec f combine nil (Fundec (x, FundecF name params result body)) =
    let (body', res) = f body
    in (Fundec (x, FundecF name params result body'), res `combine` nil)

foldExp :: (Exp t v t1 t2 t3 -> b) -> (b -> b -> b) -> b -> Exp t v t1 t2 t3 -> b
foldExp f combine nil (Exp (x, e)) =
    let f' = (\x -> (x, f x))
        foldExpsInVar = transformExpsInVar f' combine nil
        foldExpsInDec = transformExpsInDec f' combine nil
        foldExpsInFundec = transformExpsInFundec f' combine nil
    in foldExpF f' foldExpsInVar foldExpsInDec combine nil e


transformMExp f combine nil (Exp (x, e)) =
    do (e', res) <- transformMExpF f
                       (transformMExpsInVar f combine nil)
                       (transformMExpsInDec f combine nil)
                       combine nil e
       return (Exp (x, e'), res)

transformMExpsInVar f combine nil (Var (x, v)) =
    do (v', res) <- transformMVarF f (transformMExpsInVar f combine nil) combine nil v
       return (Var (x, v'), res)

transformMExpsInDec f combine nil (Dec (x, dec)) =
    do (dec', res) <- transformMDecF f (transformMExpsInFundec f combine nil)
                      (\x -> return (x, nil)) combine nil dec
       return (Dec (x, dec'), res)

transformMExpsInFundec f combine nil (Fundec (x, FundecF name params result body)) =
    do (body', res) <- f body
       return (Fundec (x, FundecF name params result body'), res `combine` nil)

foldMExp :: Monad m =>
            (Exp t v t1 t2 t3 -> m r)
         -> (r -> r -> r) -> r -> Exp t v t1 t2 t3 -> m r
foldMExp f combine nil (Exp (x, e)) =
    let f' x = f x >>= return . (,) x
        foldMExpsInVar = transformMExpsInVar f' combine nil
        foldMExpsInDec = transformMExpsInDec f' combine nil
        foldMExpsInFundec = transformMExpsInFundec f' combine nil
    in foldMExpF f' foldMExpsInVar foldMExpsInDec combine nil e


mapExp f (Exp (x, e)) =
    let mapToExpsInVar (Var (x, v)) = Var (x, mapVarF f mapToExpsInVar v)

        mapToExpsInDec (Dec (x, dec)) = Dec (x, mapDecF f mapToExpsInFundec id dec)

        mapToExpsInFundec (Fundec (x, dec)) = Fundec (x, fmap f dec)

    in Exp (x, mapExpF f mapToExpsInVar mapToExpsInDec e)

mapMExp f (Exp (x, e)) =
    let mapMToExpsInVar (Var (x, v)) = mapMVarF f mapMToExpsInVar v
                                       >>= return . (\y -> Var (x, y))

        mapMToExpsInDec (Dec (x, dec)) = mapMDecF f mapMToExpsInFundec return dec
                                         >>= return . (\y -> Dec (x, y))

        mapMToExpsInFundec (Fundec (x, FundecF name params result body)) =
            do body' <- f body
               return $ Fundec (x, FundecF name params result body')


     in mapMExpF f mapMToExpsInVar mapMToExpsInDec e >>= return . (\y -> Exp (x, y))
