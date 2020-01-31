module Test.Main where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Data.Const (Const(..))
import Data.Identity (Identity(..))
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.Unfoldable as Unfoldable
import Effect (Effect)
import Effect.Aff (Error, launchAff_)
import Record.HigherKinded (foldMapRecord, liftRecord, liftRecord', mapRecord, sequenceRecord, traverseRecord, unliftRecord, zipWithRecord)
import Test.Example as Example
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpec')

----------------------------------------------------------------

main :: Effect Unit
main = do
  let config = defaultConfig{ timeout = Nothing }
  launchAff_ $ runSpec' config [ consoleReporter ] do
    describe "RecordFunctor" do
      it "checks mapRecord functionality" do
        mapRecordTest
    describe "LiftRecord" do
      it "checks liftRecord functionality" do
        liftRecordTest
    describe "ZipRecord" do
      it "checks zipRecord functionality" do
        zipRecordTest
    describe "RecordTraversable" do
      it "checks sequenceRecord functionality" do
        sequenceRecordTest
      it "checks traverseRecord functionality" do
        traverseRecordTest
      it "checks foldMapRecord functionality" do
        foldMapRecordTest
    describe "Examples" do
      it "runs example" do
        pure Example.example
      it "runs parallel traversing example" do
        Example.parallelTraverseExample

----------------------------------------------------------------

mapRecordTest :: forall m. MonadError Error m => m Unit
mapRecordTest = do
  let source = liftRecord { x: 123, y: [ 1, 2, 3 ], z: "123" }
  do
    let toArrayRecord = mapRecord Unfoldable.fromMaybe
    let test1 = toArrayRecord source
    test1 `shouldEqual` { x: [ 123 ], y: [ [ 1, 2, 3 ] ], z: [ "123" ] }
    let source' = { x: Just 123, y: Nothing, z: Nothing }
    let test2 = toArrayRecord source'
    test2 `shouldEqual` { x: [ 123 ], y: [], z: [] }

----------------------------------------------------------------

liftRecordTest :: forall m. MonadError Error m => m Unit
liftRecordTest = do
  let source = { a: 0, b: [ 1 ], c: "a", d: Just 'b', e: { f: { g: unit } } }
  do
    let test1 = liftRecord source
    test1 `shouldEqual` { a: Just 0, b: Just [ 1 ], c: Just "a", d: Just (Just 'b'), e: Just { f: { g: unit } } }
  do
    let test2 = liftRecord source
    test2 `shouldEqual` { a: [ 0 ], b: [ [ 1 ] ], c: [ "a" ], d: [ Just 'b' ], e: [ { f: { g: unit } } ] }
  do
    let test3 = liftRecord' Identity source
    test3 `shouldEqual` { a: Identity 0, b: Identity [ 1 ], c: Identity "a", d: Identity (Just 'b'), e: Identity { f: { g: unit } } }
  do
    let test3 = liftRecord' (const $ Const 0) source
    test3 `shouldEqual` { a: Const 0, b: Const 0, c: Const 0, d: Const 0, e: Const 0 }

----------------------------------------------------------------

type MyRecord f = { x :: f Int , y :: f (Array Int) , z :: f String }

fill :: forall f. Applicative f => MyRecord f -> MyRecord Maybe -> MyRecord f
fill = zipWithRecord (maybe <@> pure)

zipRecordTest :: forall m. MonadError Error m => m Unit
zipRecordTest = do
  let default = liftRecord' Identity { x: 0, y: [], z: "" }
  do
    let test1 = fill default { x: Just 123, y: Just [ 1, 2, 3 ], z: Just "123" }
    unliftRecord test1 `shouldEqual` { x: 123, y: [ 1, 2, 3 ], z: "123" }
  do
    let test2 = fill default { x: Just 123, y: Nothing, z: Nothing }
    unliftRecord test2 `shouldEqual` { x: 123, y: [], z: "" }
  do
    let test3 = fill default { x: Nothing, y: Nothing, z: Nothing }
    unliftRecord test3 `shouldEqual` unliftRecord default

----------------------------------------------------------------

sequenceRecordTest :: forall m. MonadError Error m => m Unit
sequenceRecordTest = do
  let source = { a: 123, b: "abc" }
  let test1 = liftRecord source
  sequenceRecord test1 `shouldEqual` Just source
  let test2 = test1{ a = Nothing }
  sequenceRecord test2 `shouldEqual` (Nothing :: Maybe { a :: Int, b :: String })
  let test3 = test1{ b = Nothing }
  sequenceRecord test3 `shouldEqual` (Nothing :: Maybe { a :: Int, b :: String })

traverseRecordTest :: forall m. MonadError Error m => m Unit
traverseRecordTest = do
  let source = { a: [ 1, 2 ], b: [ 'x', 'y' ] }
  let list = traverseRecord List.fromFoldable source
  list `shouldEqual` List.fromFoldable [ { a: 1, b: 'x' }, { a: 1, b: 'y' }, { a: 2, b: 'x' }, { a: 2, b: 'y' } ]

foldMapRecordTest :: forall m. MonadError Error m => m Unit
foldMapRecordTest = do
  let source = { a: Just 123, b: Nothing :: Maybe Int, c: Nothing :: Maybe String, d: Just "abc" }
  let result = foldMapRecord (maybe "Nothing" (const "Just")) source
  result `shouldEqual` "JustNothingNothingJust"
