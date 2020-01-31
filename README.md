# purescript-higher-kinded-record
Higher-Kinded Data (HKD) support for PureScript's Record type.

```purescript
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
```
