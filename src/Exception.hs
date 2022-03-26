{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Exception () where

import Data.Either (Either(..))
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

class MonadException e m where
    throwException :: e -> m a
    catchException :: m a -> (e -> m a) -> m a
    traceException :: (e -> e) -> m a -> m a

newtype ExceptionT m e a = ExceptionT { runExceptionT :: m (Either e a)}

liftExceptionT :: (Functor m) =>  m a -> ExceptionT m e a
liftExceptionT = ExceptionT . fmap Right

instance (Monad m) => MonadException e (ExceptionT m e) where
    throwException e = ExceptionT $ return $ Left e
    catchException x f = ExceptionT $ do
        { x' <- runExceptionT x
        ; case x' of
            { Left e' -> runExceptionT $ f e'
            ; Right a' -> return $ Right a'
            }
        }
    traceException f x = ExceptionT $ fmap (either (Left . f) Right) $ runExceptionT x

instance Functor (m) => Functor (ExceptionT m e) where
    fmap f x = ExceptionT $ fmap (fmap f) $ runExceptionT x
    a <$ x = ExceptionT $ fmap (a <$) $ runExceptionT x

instance (Applicative m) => Applicative (ExceptionT m e) where
    pure x = ExceptionT $ pure $ Right x
    f <*> x = ExceptionT $ liftA2 (<*>) (runExceptionT f) (runExceptionT x)

instance (MonadIO m) => MonadIO (ExceptionT m e) where
    liftIO x = ExceptionT $ fmap Right $ liftIO x

instance MonadTrans (ExceptionT e) where
    lift = liftExceptionT

instance (Monad m) => Monad (ExceptionT m e) where
    return = liftExceptionT . return
    x >>= f = ExceptionT $ do
        { x' <- runExceptionT x
        ; case x' of
            { Left e' -> return $ Left e'
            ; Right a' -> runExceptionT $ f a'
            }
        }
