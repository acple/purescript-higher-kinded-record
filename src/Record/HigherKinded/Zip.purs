module Record.HigherKinded.Zip where

import Prelude

import Data.Functor.Product (Product, product)
import Data.Symbol (class IsSymbol, SProxy(..))
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Type.Data.RowList (RLProxy(..))

----------------------------------------------------------------

class ZipRecord rowListF rowListG fr gr hr f g h
  | rowListF -> f, rowListG -> g, rowListF rowListG h -> hr, fr -> f, gr -> g, hr -> h
  where
  zipWithRecordImpl :: RLProxy rowListF -> RLProxy rowListG -> (forall a. f a -> g a -> h a) -> { | fr } -> { | gr } -> { | hr }

instance nilZipRecord :: ZipRecord RL.Nil RL.Nil fr gr () f g h where
  zipWithRecordImpl _ _ _ _ _ = {}

instance consZipRecord ::
  ( IsSymbol key
  , Row.Cons key (f a) fr' fr
  , Row.Cons key (g a) gr' gr
  , Row.Cons key (h a) hr' hr
  , Row.Lacks key hr'
  , ZipRecord tailF tailG fr gr hr' f g h
  ) => ZipRecord (RL.Cons key (f a) tailF) (RL.Cons key (g a) tailG) fr gr hr f g h where
  zipWithRecordImpl _ _ f fr gr =
    Record.insert key (f (Record.get key fr) (Record.get key gr)) $ zipWithRecordImpl tailF tailG f fr gr
    where
    key = SProxy :: SProxy key
    tailF = RLProxy :: RLProxy tailF
    tailG = RLProxy :: RLProxy tailG

----------------------------------------------------------------

zipWithRecord
  :: forall rowListF rowListG fr gr hr f g h
  .  RL.RowToList fr rowListF
  => RL.RowToList gr rowListG
  => ZipRecord rowListF rowListG fr gr hr f g h
  => (forall a. f a -> g a -> h a) -> { | fr } -> { | gr } -> { | hr }
zipWithRecord = zipWithRecordImpl (RLProxy :: RLProxy rowListF) (RLProxy :: RLProxy rowListG)

zipRecord
  :: forall rowListF rowListG fr gr pr f g
  .  RL.RowToList fr rowListF
  => RL.RowToList gr rowListG
  => ZipRecord rowListF rowListG fr gr pr f g (Product f g)
  => { | fr } -> { | gr } -> { | pr }
zipRecord = zipWithRecord product
