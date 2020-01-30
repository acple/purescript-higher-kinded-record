module Record.HigherKinded.Traversable where

import Prelude

import Data.Const (Const(..))
import Data.Newtype (un)
import Prim.RowList as RL
import Record.Builder as Builder
import Record.HigherKinded.Internal.Traversable (class RecordTraversableInternal, traverseRecordImpl)
import Type.Data.RowList (RLProxy(..))

----------------------------------------------------------------

class RecordTraversable fr r f g | fr -> f, fr g -> r where
  traverseRecord :: (f ~> g) -> { | fr } -> g { | r }

instance instanceRecordTraversable ::
  ( RL.RowToList fr rowList
  , RecordTraversableInternal rowList fr r f g
  , Applicative g
  ) => RecordTraversable fr r f g where
  traverseRecord f fr =
    Builder.build <@> {} <$> traverseRecordImpl (RLProxy :: RLProxy rowList) f fr

sequenceRecord :: forall fr r f. RecordTraversable fr r f f => { | fr } -> f { | r }
sequenceRecord = traverseRecord identity

foldMapRecord
  :: forall fr r f m. RecordTraversable fr r f (Const m) => Monoid m
  => (forall a. f a -> m) -> { | fr } -> m
foldMapRecord f = un Const <<< traverseRecord (Const <<< f)
