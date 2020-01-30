module Record.HigherKinded.Internal.Zip where

import Prelude

import Data.Symbol (class IsSymbol, SProxy(..))
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Record.Builder as Builder
import Type.Data.RowList (RLProxy(..))

----------------------------------------------------------------

class ZipRecordInternal rowListF rowListG fr gr hr f g h
  | rowListF -> f, rowListG -> g, rowListF rowListG h -> hr, fr -> f, gr -> g, hr -> h
  where
  zipWithRecordImpl
    :: RLProxy rowListF -> RLProxy rowListG
    -> (forall a. f a -> g a -> h a) -> { | fr } -> { | gr } -> Builder.Builder {} { | hr }

instance nilZipRecord :: ZipRecordInternal RL.Nil RL.Nil fr gr () f g h where
  zipWithRecordImpl _ _ _ _ _ = identity

instance consZipRecord ::
  ( IsSymbol key
  , Row.Cons key (f a) fr' fr
  , Row.Cons key (g a) gr' gr
  , Row.Cons key (h a) hr' hr
  , Row.Lacks key hr'
  , ZipRecordInternal tailF tailG fr gr hr' f g h
  ) => ZipRecordInternal (RL.Cons key (f a) tailF) (RL.Cons key (g a) tailG) fr gr hr f g h where
  zipWithRecordImpl _ _ f fr gr =
    Builder.insert key (f (Record.get key fr) (Record.get key gr)) <<< zipWithRecordImpl tailF tailG f fr gr
    where
    key = SProxy :: SProxy key
    tailF = RLProxy :: RLProxy tailF
    tailG = RLProxy :: RLProxy tailG
