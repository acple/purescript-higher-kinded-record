module Record.HigherKinded.Traversable where

import Prelude

import Data.Const (Const(..))
import Data.Newtype (un)
import Data.Symbol (class IsSymbol, SProxy(..))
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Type.Data.RowList (RLProxy(..))

----------------------------------------------------------------

class RecordTraversable rowList fr r f g | rowList -> f, rowList g -> r, fr -> f where
  traverseRecordImpl :: RLProxy rowList -> (f ~> g) -> { | fr } -> g { | r }

instance nilRecordTraversable :: Applicative g => RecordTraversable RL.Nil fr () f g where
  traverseRecordImpl _ _ _ = pure {}

instance consRecordTraversable ::
  ( IsSymbol key
  , Row.Cons key (f a) fr' fr
  , Row.Cons key a r' r
  , Row.Lacks key r'
  , RecordTraversable tail fr r' f g
  , Applicative g
  ) => RecordTraversable (RL.Cons key (f a) tail) fr r f g where
  traverseRecordImpl _ f fr =
    Record.insert key <$> f (Record.get key fr) <*> traverseRecordImpl tail f fr
    where
    key = SProxy :: SProxy key
    tail = RLProxy :: RLProxy tail

----------------------------------------------------------------

traverseRecord
  :: forall rowList fr r f g
  .  RL.RowToList fr rowList
  => RecordTraversable rowList fr r f g
  => (f ~> g) -> { | fr } -> g { | r }
traverseRecord = traverseRecordImpl (RLProxy :: RLProxy rowList)

sequenceRecord
  :: forall rowList fr r f
  .  RL.RowToList fr rowList
  => RecordTraversable rowList fr r f f
  => { | fr } -> f { | r }
sequenceRecord = traverseRecord identity

foldMapRecord
  :: forall rowList fr r f m
  .  RL.RowToList fr rowList
  => RecordTraversable rowList fr r f (Const m)
  => Monoid m
  => (forall a. f a -> m) -> { | fr } -> m
foldMapRecord f = un Const <<< traverseRecord (Const <<< f)
