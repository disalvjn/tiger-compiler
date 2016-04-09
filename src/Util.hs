module Util(whenJust, liftState, dropState, safeLast, anyDuplicates) where
import qualified Control.Monad.RWS as RWS
import qualified Control.Monad.State.Strict as ST
import qualified Data.Set as Set

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust x f = maybe (return ()) f x

liftState :: RWS.Monoid w => ST.State s a -> RWS.RWS r w s a
liftState state = do
  init <- RWS.get
  let (a,s) = ST.runState state init
  RWS.put s
  return a

dropState :: RWS.Monoid w => RWS.RWS r w s a -> r -> ST.State s (a,w)
dropState rws read = do
  state <- ST.get
  let (result, state', w) = RWS.runRWS rws read state
  ST.put state'
  return (result, w)

safeLast :: [a] -> Maybe a
safeLast list = if null list then Nothing else Just $ last list

anyDuplicates :: Ord a => [a] -> Maybe a
anyDuplicates list =
    let go list set =
            case list of
              [] -> Nothing
              (x:xs) -> if Set.member x set
                        then Just x
                        else go xs $ Set.insert x set
    in go list Set.empty
