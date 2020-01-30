module Record.HigherKinded.Zip where

import Prelude

import Data.Functor.Product (Product, product)
import Prim.RowList as RL
import Record.Builder as Builder
import Record.HigherKinded.Internal.Zip (class ZipRecordInternal, zipWithRecordImpl)
import Type.Data.RowList (RLProxy(..))

----------------------------------------------------------------

class ZipRecord fr gr hr f g h | fr -> f, gr -> g, hr -> h, fr gr h -> hr where
  zipWithRecord :: (forall a. f a -> g a -> h a) -> { | fr } -> { | gr } -> { | hr }

instance instanceZipRecord ::
  ( RL.RowToList fr rowListF
  , RL.RowToList gr rowListG
  , ZipRecordInternal rowListF rowListG fr gr hr f g h
  ) => ZipRecord fr gr hr f g h where
  zipWithRecord f fr gr = Builder.build <@> {} $ zipWithRecordImpl rowListF rowListG f fr gr
    where
    rowListF = RLProxy :: RLProxy rowListF
    rowListG = RLProxy :: RLProxy rowListG

zipRecord
  :: forall fr gr pr f g. ZipRecord fr gr pr f g (Product f g)
  => { | fr } -> { | gr } -> { | pr }
zipRecord = zipWithRecord product
