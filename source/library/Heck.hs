{-# LANGUAGE RankNTypes #-}

module Heck where

import qualified Control.Monad as Monad
import qualified GHC.Stack as Stack

data Test m n = MkTest
  { assertFailure :: forall x. (Stack.HasCallStack) => String -> m x,
    describe :: String -> n () -> n (),
    it :: String -> m () -> n ()
  }

assertEq :: (Stack.HasCallStack, Applicative m, Eq a, Show a) => Test m n -> a -> a -> m ()
assertEq t x y = Monad.unless (x == y) . assertFailure t $ "expected: " <> show x <> " == " <> show y

assertNe :: (Stack.HasCallStack, Applicative m, Eq a, Show a) => Test m n -> a -> a -> m ()
assertNe t x y = Monad.unless (x /= y) . assertFailure t $ "expected: " <> show x <> " /= " <> show y

assertLt :: (Stack.HasCallStack, Applicative m, Ord a, Show a) => Test m n -> a -> a -> m ()
assertLt t x y = Monad.unless (x < y) . assertFailure t $ "expected: " <> show x <> " < " <> show y

assertLe :: (Stack.HasCallStack, Applicative m, Ord a, Show a) => Test m n -> a -> a -> m ()
assertLe t x y = Monad.unless (x <= y) . assertFailure t $ "expected: " <> show x <> " <= " <> show y

assertGt :: (Stack.HasCallStack, Applicative m, Ord a, Show a) => Test m n -> a -> a -> m ()
assertGt t x y = Monad.unless (x > y) . assertFailure t $ "expected: " <> show x <> " > " <> show y

assertGe :: (Stack.HasCallStack, Applicative m, Ord a, Show a) => Test m n -> a -> a -> m ()
assertGe t x y = Monad.unless (x >= y) . assertFailure t $ "expected: " <> show x <> " >= " <> show y
