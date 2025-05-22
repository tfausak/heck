{-# LANGUAGE RankNTypes #-}

module Heck where

import qualified Control.Monad as Monad
import qualified GHC.Stack as Stack

data Test m n = MkTest
  { assertFailure :: forall x. (Stack.HasCallStack) => String -> m x,
    describe :: String -> n () -> n (),
    it :: String -> m () -> n ()
  }

assertEq ::
  (Stack.HasCallStack, Applicative m, Eq a, Show a) =>
  Test m n ->
  -- | expected
  a ->
  -- | actual
  a ->
  m ()
assertEq test expected actual =
  Monad.when (expected /= actual)
    . assertFailure test
    $ "expected " <> show expected <> " but got " <> show actual
