module Record.HigherKinded.Internal.Traversable where

import Prelude

import Data.Symbol (class IsSymbol, SProxy(..))
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Record.Builder as Builder
import Type.Data.RowList (RLProxy(..))

----------------------------------------------------------------

class RecordTraversableInternal rowList fr r f g | rowList -> f, rowList g -> r, fr -> f where
  traverseRecordImpl :: RLProxy rowList -> (f ~> g) -> { | fr } -> g (Builder.Builder {} { | r })

instance nilRecordTraversable :: Applicative g => RecordTraversableInternal RL.Nil fr () f g where
  traverseRecordImpl _ _ _ = pure identity

instance consRecordTraversable ::
  ( IsSymbol key
  , Row.Cons key (f a) fr' fr
  , Row.Cons key a r' r
  , Row.Lacks key r'
  , RecordTraversableInternal tail fr r' f g
  , Applicative g
  ) => RecordTraversableInternal (RL.Cons key (f a) tail) fr r f g where
  traverseRecordImpl _ f fr =
    (<<<) <$> Builder.insert key <$> f (Record.get key fr) <*> traverseRecordImpl tail f fr
    where
    key = SProxy :: SProxy key
    tail = RLProxy :: RLProxy tail
