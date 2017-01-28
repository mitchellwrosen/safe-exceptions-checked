{-# language DeriveDataTypeable #-}
{-# language FlexibleContexts   #-}
{-# language RankNTypes         #-}

import Control.Exception.Safe.Checked

import Control.Applicative
import Data.Typeable
import Test.Hspec

data Foo = Foo
  deriving (Eq, Show, Typeable)
instance Exception Foo

throwsFoo :: Throws Foo => IO ()
throwsFoo = throw Foo

doesntThrowFoo :: Throws Foo => IO ()
doesntThrowFoo = pure ()

throwsImpureFoo :: ThrowsImpure Foo => Bool
throwsImpureFoo = impureThrow Foo

doesntThrowImpureFoo :: ThrowsImpure Foo => Bool
doesntThrowImpureFoo = True

catchesFoo :: (Throws Foo => IO a) -> (Foo -> IO a) -> IO a
catchesFoo = catch

catchesImpureFoo :: NFData a => (ThrowsImpure Foo => IO a) -> (Foo -> IO a) -> IO a
catchesImpureFoo = catchDeep

main :: IO ()
main = hspec $ do
  it "throw, catch" $ do
    catchesFoo (throwsFoo >> pure True) (\_ -> pure False)
      `shouldReturn` False

  it "no throw, no catch" $ do
    catchesFoo (doesntThrowFoo >> pure True) (\_ -> pure False)
      `shouldReturn` True

  it "throw impure, catch" $ do
    catchesImpureFoo (pure throwsImpureFoo) (\_ -> pure False)
      `shouldReturn` False

  it "no throw impure, no catch" $ do
    catchesImpureFoo (pure doesntThrowImpureFoo) (\_ -> pure False)
      `shouldReturn` True

  it "try throw" $ do
    try throwsFoo
      `shouldReturn` Left Foo

  it "try no throw" $ do
    try doesntThrowFoo
      `shouldReturn` (Right () :: Either Foo ())

  it "try throw impure" $ do
    tryDeep (pure throwsImpureFoo)
      `shouldReturn` Left Foo

  it "try no throw impure" $ do
    tryDeep (pure doesntThrowImpureFoo)
      `shouldReturn` (Right True :: Either Foo Bool)
