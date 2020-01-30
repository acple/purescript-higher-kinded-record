module Record.HigherKinded.Internal.Functor where

import Prelude

import Data.Symbol (class IsSymbol, SProxy(..))
import Prim.Row as Row
import Prim.RowList as RL
import Record.Builder as Builder
import Type.Data.RowList (RLProxy(..))

----------------------------------------------------------------

class RecordFunctorInternal rowList fr gr f g | rowList -> f, rowList g -> gr, fr -> f, gr -> g where
  mapRecordImpl :: RLProxy rowList -> (f ~> g) -> Builder.Builder { | fr } { | gr }

instance nilRecordFunctor :: RecordFunctorInternal RL.Nil fr fr f g where
  mapRecordImpl _ _ = identity

instance consRecordFunctor ::
  ( IsSymbol key
  , Row.Cons key (f a) r gr'
  , Row.Cons key (g a) r gr
  , RecordFunctorInternal tail fr gr' f g
  ) => RecordFunctorInternal (RL.Cons key (f a) tail) fr gr f g where
  mapRecordImpl _ f = Builder.modify key f <<< mapRecordImpl tail f
    where
    key = SProxy :: SProxy key
    tail = RLProxy :: RLProxy tail
