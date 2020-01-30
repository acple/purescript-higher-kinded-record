module Record.HigherKinded.Lift where

import Prelude

import Control.Comonad (class Comonad, extract)
import Data.Identity (Identity(..))
import Data.Newtype (un)
import Data.Symbol (class IsSymbol, SProxy(..))
import Prim.Row as Row
import Prim.RowList as RL
import Record.Builder as Builder
import Record.HigherKinded.Traversable (class RecordTraversable, traverseRecord)
import Type.Data.RowList (RLProxy(..))

----------------------------------------------------------------

class LiftRecord rowList r fr f | rowList f -> fr, fr -> f where
  liftRecordImpl :: RLProxy rowList -> (forall a. a -> f a) -> Builder.Builder { | r } { | fr }

instance nilLiftRecord :: LiftRecord RL.Nil r r f where
  liftRecordImpl _ _ = identity

instance consLiftRecord ::
  ( IsSymbol key
  , Row.Cons key a r' fr'
  , Row.Cons key (f a) r' fr
  , LiftRecord tail r fr' f
  ) => LiftRecord (RL.Cons key a tail) r fr f where
  liftRecordImpl _ f = Builder.modify key f <<< liftRecordImpl tail f
    where
    key = SProxy :: SProxy key
    tail = RLProxy :: RLProxy tail

----------------------------------------------------------------

liftRecord'
  :: forall rowList r fr f
  .  RL.RowToList r rowList
  => LiftRecord rowList r fr f
  => (forall a. a -> f a) -> { | r } -> { | fr }
liftRecord' f = Builder.build $ liftRecordImpl (RLProxy :: RLProxy rowList) f

liftRecord
  :: forall rowList r fr f
  .  RL.RowToList r rowList
  => LiftRecord rowList r fr f
  => Applicative f
  => { | r } -> { | fr }
liftRecord = liftRecord' pure

unliftRecord'
  :: forall rowList fr r f
  .  RL.RowToList fr rowList
  => RecordTraversable rowList fr r f Identity
  => (forall a. f a -> a) -> { | fr } -> { | r }
unliftRecord' f = un Identity <<< traverseRecord (pure <<< f)

unliftRecord
  :: forall rowList fr r f
  .  RL.RowToList fr rowList
  => RecordTraversable rowList fr r f Identity
  => Comonad f
  => { | fr } -> { | r }
unliftRecord = unliftRecord' extract
