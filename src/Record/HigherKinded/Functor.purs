module Record.HigherKinded.Functor where

import Prelude

import Prim.RowList as RL
import Record.Builder as Builder
import Record.HigherKinded.Internal.Functor (class RecordFunctorInternal, mapRecordImpl)
import Type.Data.RowList (RLProxy(..))

----------------------------------------------------------------

class RecordFunctor fr gr f g | fr -> f, gr -> g, fr g -> gr, gr f -> fr where
  mapRecord :: (f ~> g) -> { | fr } -> { | gr }

instance instanceRecordFunctor ::
  ( RL.RowToList fr rowList
  , RecordFunctorInternal rowList fr gr f g
  ) => RecordFunctor fr gr f g where
  mapRecord f = Builder.build $ mapRecordImpl (RLProxy :: RLProxy rowList) f
