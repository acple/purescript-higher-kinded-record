module Record.HigherKinded where

import Prelude

import Control.Comonad (class Comonad, extract)
import Data.Const (Const(..))
import Data.Functor.Product (Product, product)
import Data.Identity (Identity(..))
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (un)
import Prim.RowList as RL
import Record.Builder as Builder
import Record.HigherKinded.Internal as Internal
import Type.Data.RowList (RLProxy(..))

----------------------------------------------------------------

class RecordFunctor fr gr f g | fr -> f, gr -> g, fr g -> gr, gr f -> fr where
  mapRecord :: (f ~> g) -> { | fr } -> { | gr }

instance instanceRecordFunctor ::
  ( RL.RowToList fr rowList
  , Internal.RecordFunctorInternal rowList fr gr f g
  ) => RecordFunctor fr gr f g where
  mapRecord f = Builder.build $ Internal.mapRecordImpl rowList f
    where
    rowList = RLProxy :: RLProxy rowList

----------------------------------------------------------------

class RecordTraversable fr r f g | fr -> f, fr g -> r where
  traverseRecord :: (f ~> g) -> { | fr } -> g { | r }

instance instanceRecordTraversable ::
  ( RL.RowToList fr rowList
  , Internal.RecordTraversableInternal rowList fr r f g
  , Applicative g
  ) => RecordTraversable fr r f g where
  traverseRecord f fr = Builder.build <@> {} <$> Internal.traverseRecordImpl rowList f fr
    where
    rowList = RLProxy :: RLProxy rowList

sequenceRecord :: forall fr r f. RecordTraversable fr r f f => { | fr } -> f { | r }
sequenceRecord = traverseRecord identity

foldMapRecord
  :: forall fr r f m. RecordTraversable fr r f (Const m) => Monoid m
  => (forall a. f a -> m) -> { | fr } -> m
foldMapRecord f = un Const <<< traverseRecord (Const <<< f)

traverseRecord_
  :: forall fr r f g b. RecordTraversable fr r f (Const (Endo (->) (g Unit))) => Applicative g
  => (forall a. f a -> g b) -> { | fr } -> g Unit
traverseRecord_ f = (un Endo <@> pure unit) <<< foldMapRecord (Endo <<< (*>) <<< f)

----------------------------------------------------------------

class LiftRecord r fr f | fr -> r f, r f -> fr where
  liftRecord' :: (forall a. a -> f a) -> { | r } -> { | fr }

instance instanceLiftRecord ::
  ( RL.RowToList r rowList
  , Internal.LiftRecordInternal rowList r fr f
  ) => LiftRecord r fr f where
  liftRecord' f = Builder.build $ Internal.liftRecordImpl rowList f
    where
    rowList = RLProxy :: RLProxy rowList

liftRecord :: forall r fr f. LiftRecord r fr f => Applicative f => { | r } -> { | fr }
liftRecord = liftRecord' pure

unliftRecord' :: forall fr r f. RecordTraversable fr r f Identity => (forall a. f a -> a) -> { | fr } -> { | r }
unliftRecord' f = un Identity <<< traverseRecord (pure <<< f)

unliftRecord :: forall fr r f. RecordTraversable fr r f Identity => Comonad f => { | fr } -> { | r }
unliftRecord = unliftRecord' extract

----------------------------------------------------------------

class ZipRecord fr gr hr f g h | fr -> f, gr -> g, hr -> h, fr gr h -> hr where
  zipWithRecord :: (forall a. f a -> g a -> h a) -> { | fr } -> { | gr } -> { | hr }

instance instanceZipRecord ::
  ( RL.RowToList fr rowListF
  , RL.RowToList gr rowListG
  , Internal.ZipRecordInternal rowListF rowListG fr gr hr f g h
  ) => ZipRecord fr gr hr f g h where
  zipWithRecord f fr gr = Builder.build <@> {} $ Internal.zipWithRecordImpl rowListF rowListG f fr gr
    where
    rowListF = RLProxy :: RLProxy rowListF
    rowListG = RLProxy :: RLProxy rowListG

zipRecord :: forall fr gr pr f g. ZipRecord fr gr pr f g (Product f g) => { | fr } -> { | gr } -> { | pr }
zipRecord = zipWithRecord product
