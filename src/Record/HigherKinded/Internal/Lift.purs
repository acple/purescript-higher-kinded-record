module Record.HigherKinded.Internal.Lift where

import Prelude

import Data.Symbol (class IsSymbol, SProxy(..))
import Prim.Row as Row
import Prim.RowList as RL
import Record.Builder as Builder
import Type.Data.RowList (RLProxy(..))

----------------------------------------------------------------

class LiftRecordInternal rowList r fr f | rowList f -> fr, fr -> f where
  liftRecordImpl :: RLProxy rowList -> (forall a. a -> f a) -> Builder.Builder { | r } { | fr }

instance nilLiftRecord :: LiftRecordInternal RL.Nil r r f where
  liftRecordImpl _ _ = identity

instance consLiftRecord ::
  ( IsSymbol key
  , Row.Cons key a r' fr'
  , Row.Cons key (f a) r' fr
  , LiftRecordInternal tail r fr' f
  ) => LiftRecordInternal (RL.Cons key a tail) r fr f where
  liftRecordImpl _ f = Builder.modify key f <<< liftRecordImpl tail f
    where
    key = SProxy :: SProxy key
    tail = RLProxy :: RLProxy tail
