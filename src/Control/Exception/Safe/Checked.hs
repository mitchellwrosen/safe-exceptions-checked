{-# language CPP                 #-}
{-# language FlexibleContexts    #-}
{-# language IncoherentInstances #-}
{-# language RankNTypes          #-}
{-# language ScopedTypeVariables #-}

#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE RoleAnnotations #-}
#endif

-- | Lightweight checked exceptions, based on
-- <https://www.well-typed.com/blog/2015/07/checked-exceptions/>.

module Control.Exception.Safe.Checked
    ( -- * Throwing
      Throws
    , ThrowsImpure
    , throw
    , impureThrow
      -- * Catching
    , catch
    , catchDeep
    , handle
    , handleDeep
    , try
    , tryDeep
      -- * Unchecking exceptions
    , uncheck
    , uncheckImpure
      -- * Re-exports
    , Exception
    , MonadIO
    , MonadCatch
    , MonadMask
    , MonadThrow
    , NFData
    ) where

import Control.DeepSeq (NFData)
import Control.Exception (Exception(..))
import Control.Exception.Safe (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Proxy (Proxy(Proxy))

import qualified Control.Exception.Safe as Safe

#if __GLASGOW_HASKELL__ >= 708
import Data.Coerce (coerce)
#else
import Unsafe.Coerce (unsafeCoerce)
#endif

-- | Like 'Control.Exception.Safe.throw', but for checked exceptions.
--
-- @since 0.1.0
throw :: (MonadThrow m, Exception e, Throws e) => e -> m a
throw = Safe.throw

-- | Like 'Control.Exception.Safe.impureThrow', but for checked exceptions.
--
-- @since 0.1.0
impureThrow :: (Exception e, ThrowsImpure e) => e -> a
impureThrow = Safe.impureThrow

-- | Like 'Control.Exception.Safe.catch', but for checked exceptions.
--
-- @since 0.1.0
catch :: (MonadCatch m, Exception e) => (Throws e => m a) -> (e -> m a) -> m a
catch = catch_
 where
  catch_
    :: forall a e m. (MonadCatch m, Exception e)
    => (Throws e => m a) -> (e -> m a) -> m a
  catch_ m = Safe.catch (uncheck (Proxy :: Proxy e) m)

-- | Like 'Control.Exception.Safe.catchDeep', but for checked exceptions.
--
-- @since 0.1.0
catchDeep
  :: (MonadCatch m, MonadIO m, Exception e, NFData a)
  => (ThrowsImpure e => m a) -> (e -> m a) -> m a
catchDeep = catchDeep_
  where
    catchDeep_ :: forall a e m. (MonadCatch m, MonadIO m, Exception e, NFData a)
               => (ThrowsImpure e => m a) -> (e -> m a) -> m a
    catchDeep_ m = Safe.catchDeep (uncheckImpure (Proxy :: Proxy e) m)

-- | Like 'Control.Exception.Safe.handle', but for checked exceptions.
--
-- @since 0.1.0
handle :: (MonadCatch m, Exception e) => (e -> m a) -> (Throws e => m a) -> m a
handle f g = catch g f

-- | Like 'Control.Exception.Safe.handleDeep', but for checked exceptions.
--
-- @since 0.1.0
handleDeep
  :: (MonadCatch m, MonadIO m, Exception e, NFData a)
  => (e -> m a) -> (ThrowsImpure e => m a) -> m a
handleDeep f g = catchDeep g f

-- | Like 'Control.Exception.Safe.try', but for checked exceptions.
--
-- @since 0.1.0
try
  :: (MonadCatch m, Exception e)
  => (Throws e => m a) -> m (Either e a)
try = try_
  where
    try_
      :: forall a e m.
         (MonadCatch m, Exception e)
      => (Throws e => m a) -> m (Either e a)
    try_ m = Safe.try (uncheck (Proxy :: Proxy e) m)

-- | Like 'Control.Exception.Safe.tryDeep', but for checked exceptions.
--
-- @since 0.1.0
tryDeep :: (MonadCatch m, MonadIO m, Exception e, NFData a)
        => (ThrowsImpure e => m a) -> m (Either e a)
tryDeep = tryDeep_
  where
    tryDeep_ :: forall a e m. (MonadCatch m, MonadIO m, Exception e, NFData a)
             => (ThrowsImpure e => m a) -> m (Either e a)
    tryDeep_ m = Safe.tryDeep (uncheckImpure (Proxy :: Proxy e) m)

--------------------------------------------------------------------------------
-- Throws/ThrowsImpure machinery

-- Unexported superclass of 'Throws' to prevent other instances.
class X e

-- | A @'Throws' e@ constraint indicates a computation may throw synchronous
-- exception @e@. Introduce a constraint with 'throw', and discharge it with
-- 'catch'.
--
-- You may ignore the @X@ superclass; it exists only to prevent additional
-- 'Throws' instances from being created.
class X e => Throws e

-- | A @'ThrowsImpure' e@ constraint indicates a computation may throw impure
-- exception @e@. Introduce a constraint with 'impureThrow', and discharge it
-- with 'catchDeep'.
--
-- You may ignore the @X@ superclass; it exists only to prevent additional
-- 'ThrowsImpure' instances from being created.
class X e => ThrowsImpure e

#if __GLASGOW_HASKELL__ >= 708
type role X representational
type role Throws representational
type role ThrowsImpure representational
#endif

newtype Wrap e a = Wrap { unWrap :: Throws e => a }
newtype WrapImpure e a = WrapImpure { unWrapImpure :: ThrowsImpure e => a }

newtype Catch a = Catch a

instance X (Catch a)
instance Throws (Catch a)
instance ThrowsImpure (Catch a)

coerceWrap :: Wrap e a -> Wrap (Catch e) a
#if __GLASGOW_HASKELL__ >= 708
coerceWrap = coerce
#else
coerceWrap = unsafeCoerce
#endif

coerceWrapImpure :: WrapImpure e a -> WrapImpure (Catch e) a
#if __GLASGOW_HASKELL__ >= 708
coerceWrapImpure = coerce
#else
coerceWrapImpure = unsafeCoerce
#endif

-- | Uncheck a checked exception.
--
-- This is exported for completeness, but normally you should discharge a
-- 'Throws' constraint with 'catch'.
--
-- @since 0.1.0
uncheck :: forall a e proxy. proxy e -> (Throws e => a) -> a
uncheck _ m = unWrap (coerceWrap (Wrap m :: Wrap e a))

-- | Unchecked a checked, impure exception.
--
-- This is exported for completeness, but normally you should discharge a
-- 'ThrowsImpure' constraint with 'catchDeep'.
--
-- @since 0.1.0
uncheckImpure :: forall a e proxy. proxy e -> (ThrowsImpure e => a) -> a
uncheckImpure _ m = unWrapImpure (coerceWrapImpure (WrapImpure m :: WrapImpure e a)) -- (WrapImpure :: (ThrowsImpure e => a) -> WrapImpure e a)
