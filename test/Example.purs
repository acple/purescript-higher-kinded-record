module Test.Example where

import Prelude

import Data.DateTime as DT
import Data.Identity (Identity)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Maybe.Last (Last(..))
import Effect.Aff (Aff, Milliseconds(..), delay, parallel, sequential)
import Effect.Class (liftEffect)
import Effect.Now as Now
import Record.HigherKinded (liftRecord, liftRecord', mapRecord, sequenceRecord, traverseRecord, unliftRecord, zipRecord)
import Test.Spec.Assertions (shouldEqual, shouldNotSatisfy, shouldSatisfy)

----------------------------------------------------------------

type PlainRecord = { a :: Int, b :: String, c :: Maybe String }

type HKDRecord f = { a :: f Int, b :: f String, c :: f (Maybe String) }

example :: Unit
example = do

  let plain = { a: 123, b: "abc", c: Nothing } :: PlainRecord

  let hkdMaybe = liftRecord plain :: HKDRecord Maybe -- { a :: Maybe Int, b :: Maybe String, c :: Maybe (Maybe String) }
  let hkdArray = liftRecord plain :: HKDRecord Array -- { a :: Array Int, b :: Array String, c :: Array (Maybe String) }
  let hkdIdentity = liftRecord plain :: HKDRecord Identity -- { a :: Identity Int, b :: Identity String, c :: Identity (Maybe String) }
  let hkdLast = liftRecord' (Last <<< Just) plain :: HKDRecord Last

  let plain' = unliftRecord hkdIdentity :: PlainRecord

  -- { a :: Array _, b :: Array _, ... } -> { a :: List _, b :: List _, ... }
  let mapping = mapRecord List.fromFoldable hkdArray :: HKDRecord List -- { a :: List Int, b :: List String, c :: List (Maybe String) }

  -- { a :: Maybe _, b :: Maybe _, ... } -> Maybe { a :: _, b :: _, ... }
  let traversing = traverseRecord identity hkdMaybe :: Maybe PlainRecord -- Maybe { a :: Int, b :: String, c :: Maybe String }

  -- HKDRecord f -> HKDRecord g -> HKDRecord (Product f g)
  let zipping = zipRecord hkdMaybe hkdArray -- :: { a :: Product Maybe Array Int, b :: Product Maybe Array String, c :: Product Maybe Array (Maybe String) }

  unit

----------------------------------------------------------------

parallelTraverseExample :: Aff Unit
parallelTraverseExample = do

  let runParallel = sequential <<< traverseRecord parallel
  parallelDuration <- runWith runParallel
  parallelDuration `shouldSatisfy` (_ <= Milliseconds 500.0)

  let runSequential = sequenceRecord
  sequentialDuration <- runWith runSequential
  sequentialDuration `shouldNotSatisfy` (_ <= Milliseconds 500.0)

  where
  withDelay :: forall a. a -> Aff a
  withDelay a = delay (Milliseconds 200.0) $> a

  runWith traverse = do
    let source = { a: withDelay 123, b: withDelay [ 1, 2, 3 ], c: withDelay "abc" }

    before <- liftEffect Now.nowDateTime
    result <- traverse source
    after <- liftEffect Now.nowDateTime

    result `shouldEqual` { a: 123, b: [ 1, 2, 3 ], c: "abc" }
    pure $ DT.diff after before
