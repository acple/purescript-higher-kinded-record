module Record.HigherKinded.Lift where

import Prelude

import Control.Comonad (class Comonad, extract)
import Data.Identity (Identity(..))
import Data.Newtype (un)
import Prim.RowList as RL
import Record.Builder as Builder
import Record.HigherKinded.Internal.Lift (class LiftRecordInternal, liftRecordImpl)
import Record.HigherKinded.Traversable (class RecordTraversable, traverseRecord)
import Type.Data.RowList (RLProxy(..))

----------------------------------------------------------------

class LiftRecord r fr f | fr -> f, r f -> fr where
  liftRecord' :: (forall a. a -> f a) -> { | r } -> { | fr }

instance instanceLiftRecord ::
  ( RL.RowToList r rowList
  , LiftRecordInternal rowList r fr f
  ) => LiftRecord r fr f where
  liftRecord' f = Builder.build $ liftRecordImpl (RLProxy :: RLProxy rowList) f

liftRecord :: forall r fr f. LiftRecord r fr f => Applicative f => { | r } -> { | fr }
liftRecord = liftRecord' pure

unliftRecord' :: forall fr r f. RecordTraversable fr r f Identity => (forall a. f a -> a) -> { | fr } -> { | r }
unliftRecord' f = un Identity <<< traverseRecord (pure <<< f)

unliftRecord :: forall fr r f. RecordTraversable fr r f Identity => Comonad f => { | fr } -> { | r }
unliftRecord = unliftRecord' extract
