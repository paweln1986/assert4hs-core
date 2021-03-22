# assert4hs

 This library aims to provide a set of combinators to assert arbitrary nested data structures.
 The inspiration of this library is AssertJ for Java, the composition of assertions was inspired by `lens` library.

 New assertions can be easily written and composed with other assertions. All failed assertions are gathered and presented to the user.

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

#### Examples

##### Simple assertion

```haskell
  result = 10
  assertThat result $ isEqual 10
```

##### Composing assertion

 Assertions are composable, this allows verifying multiple conditions during one test case.

```haskell
  result = 10
  assertThat result $ 
      isGreaterThan 5 
      . isLowerThan 20
 
 
 >>> given 4 should be greater than 5
```

##### Focusing on part of data structure

 Sometimes it is convenient to transform the subject under test and execute assertions on the extracted part of it. For this purpose, we have a `focus` function.

```haskell
  data Foo = Foo {name :: String, age :: Int} deriving (Show, Eq)

  assertThat (Foo "someName" 15) $
      isEqualTo (Foo "someName" 15)
      . focus age
      . isGreaterThan 20
      . isLowerEqualThan 5

  >>> given 15 should be greater than 20 
  >>> given 15 should be lower or equal to 5
```

##### Changing subject uder test

The `focus` function allows to transform the subject under test, but the original subject is lost. Function `inside` is similar to the function `focus`, but preserve theoriginal subject under test.

```haskell
data Foo = Foo {name :: String, age :: Int} deriving (Show, Eq)

assertThat (Foo "someName" 15) $
    inside age (isGreaterThan 20 . isLowerEqualThan 5)
    . focus name 
    . isEqualTo "someName1" 

>>> given 15 should be greater than 20
>>> 
>>> given 15 should be lower or equal to 5
>>> 
>>> given "someName" should be equal to "someName1"
>>> "someName"
>>> ╷
>>> │
>>> ╵
>>> "someName1"
>>>          ▲
```

##### Tagging assertions

Once our test grows, it is hard to spot which assertions failed and why. That is why function `tag` exists, one can name assertion and give it a more readable name for failure message.

```haskell

data Foo = Foo {name :: String, age :: Int} deriving (Show, Eq)

assertThat (Foo "someName" 15) $
  inside age (tag "age" . isGreaterThan 20 . isLowerEqualThan 5)
    . tag "name"
    . focus name
    . isEqualTo "someName1"
    . tag "should not be equal"
    . isNotEqualTo "someName"

>>> [age] given 15 should be greater than 20
>>> 
>>> [age] given 15 should be lower or equal to 5
>>> 
>>> [name] given "someName" should be equal to "someName1"
>>> "someName"
>>> ╷
>>> │
>>> ╵
>>> "someName1"
>>>          ▲
>>> 
>>> [name.should not be equal] given "someName" should be not equal to "someName"
```

##### Custom assertions

 It is sometimes convenient to create a custom assertion which explicitly describes what is testing. For this purpose we have `simpleAssertion` function

```haskell
isSuitableForEmployment :: Assertion Foo
isSuitableForEmployment =
    simpleAssertion (\a -> age a > 17) (\a -> "new employee must be 18 years or older, but it has " <> show (age a))
    . simpleAssertion (\a -> age a < 70) (\a -> "must be younger than 70 years old, but it has " <> show (age a))

assertThat (Foo "someName" 15) isSuitableForEmployment

>>> new employee must be 18 years or older, but it has 15

assertThat (Foo "someName" 76) isSuitableForEmployment

>>> must be younger than 70 years old, but it has 76

```

#### Related projects

[assert4hs-tasty](https://github.com/paweln1986/assert4hs-tasty) - assert4hs provider for tasty