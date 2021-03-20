# assert4hs
 This library aims to provide a set of combinators to assert arbitrary nested data structures.
 The inspiration of this library is AssertJ for Java, the composition of assertions was inspired by `lens` library.

 #### Example:

 ```haskell
  data Foo = Foo {name :: String, age :: Int} deriving (Show, Eq)

  assertThat (Foo "someName" 15) $
       isEqualTo (Foo "someN1ame" 15)
       . focus age
       . tag "age"
       . isGreaterThan 20
 ```

 result in

  ```haskell
  given Foo {name = "someName", age = 15} should be equal to Foo {name = "someN1ame", age = 15}
  Foo {name = "someName", age = 15}
  ╷
  │
  ╵
  Foo {name = "someN1ame", age = 15}
                    ▲
  [age] given 15 should be greater than 20
 ```

 