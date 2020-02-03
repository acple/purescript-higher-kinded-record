module Record.HigherKinded.Internal where

import Prelude

import Data.Symbol (class IsSymbol, SProxy(..))
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
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

----------------------------------------------------------------

class RecordTraversableInternal rowList fr r f g | rowList -> f, rowList g -> r, fr -> f where
  traverseRecordImpl :: RLProxy rowList -> (f ~> g) -> { | fr } -> g (Builder.Builder {} { | r })

instance nilRecordTraversable :: Applicative g => RecordTraversableInternal RL.Nil fr () f g where
  traverseRecordImpl _ _ _ = pure identity

instance consRecordTraversable ::
  ( IsSymbol key
  , Row.Cons key (f a) fr' fr
  , Row.Cons key a r' r
  , Row.Lacks key r'
  , RecordTraversableInternal tail fr r' f g
  , Applicative g
  ) => RecordTraversableInternal (RL.Cons key (f a) tail) fr r f g where
  traverseRecordImpl _ f fr =
    (<<<) <$> Builder.insert key <$> f (Record.get key fr) <*> traverseRecordImpl tail f fr
    where
    key = SProxy :: SProxy key
    tail = RLProxy :: RLProxy tail

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
