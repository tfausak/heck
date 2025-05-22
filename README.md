# Heck

Heck is a Haskell library that provides a abstract unit test interface. It can
be used to write tests without depending on any particular test library.

## Example

``` hs
import Heck

spec :: (Applicative m, Monad n) => Test m n -> n ()
spec t = describe t "something" $ do
  it t "works" $ do
    assertEq t 'b' (max 'a' 'b')
  it t "fails" $ do
    assertEq t "expected" "actual"
```
