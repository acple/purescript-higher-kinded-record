module Record.HigherKinded.Functor where

import Prelude

import Data.Symbol (class IsSymbol, SProxy(..))
import Prim.Row as Row
import Prim.RowList as RL
import Record.Builder as Builder
import Type.Data.RowList (RLProxy(..))

----------------------------------------------------------------

class RecordFunctor rowList fr gr f g | rowList -> f, rowList g -> gr, fr -> f, gr -> g where
  mapRecordImpl :: RLProxy rowList -> (f ~> g) -> Builder.Builder { | fr } { | gr }

instance nilRecordFunctor :: RecordFunctor RL.Nil fr fr f g where
  mapRecordImpl _ _ = identity

instance consRecordFunctor ::
  ( IsSymbol key
  , Row.Cons key (f a) r gr'
  , Row.Cons key (g a) r gr
  , RecordFunctor tail fr gr' f g
  ) => RecordFunctor (RL.Cons key (f a) tail) fr gr f g where
  mapRecordImpl _ f = Builder.modify key f <<< mapRecordImpl tail f
    where
    key = SProxy :: SProxy key
    tail = RLProxy :: RLProxy tail

----------------------------------------------------------------

mapRecord
  :: forall rowList fr gr f g
  .  RL.RowToList fr rowList
  => RecordFunctor rowList fr gr f g
  => (f ~> g) -> { | fr } -> { | gr }
mapRecord f = Builder.build $ mapRecordImpl (RLProxy :: RLProxy rowList) f
