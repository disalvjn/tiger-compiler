module Util(whenJust, liftState) where
import qualified Control.Monad.RWS as RWS
import qualified Control.Monad.State.Strict as ST

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust x f = maybe (return ()) f x

liftState :: RWS.Monoid w => ST.State s a -> RWS.RWS r w s a
liftState state = do
  init <- RWS.get
  let (a,s) = ST.runState state init
  RWS.put s
  return a
