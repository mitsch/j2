module Exception () where

import Data.Either (Either(..))
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

class MonadException e m | m -> e where
    throwException :: e -> m a
    catchException :: m a -> (e -> m a) -> m a
    traceException :: (e -> e) -> m a -> m a

newtype ExceptionT e m a = ExceptionT { runExceptionT :: m (Either e a)}

liftExceptionT :: (Functor m) =>  m a -> ExceptionT e m a
liftExceptionT = ExceptionT . fmap Right

instance (Monad m) => MonadException e (ExceptionT e m) where
    throwException e = ExceptionT $ return $ Left e
    catchException x f = ExceptionT $ do
        { x' <- runExceptionT x
        ; case x' of
            { Left e' -> f e'
            ; Right a' -> return $ Right a'
            }
        }
    traceException f x = ExceptionT $ fmap (either f id) $ runExceptionT x

instance Functor (m) => Functor (ExceptionT e m) where
    fmap f x = ExceptionT $ fmap (fmap f) $ runExceptionT x
    a <$ x = ExceptionT $ fmap (a <$) $ runExceptionT x

instance (Applicative m) => Applicative (ExceptionT e m) where
    pure x = ExceptionT $ pure $ Right x
    liftA2 f x y = ExceptionT $ liftA2 (liftA2 f) (runExceptionT x) (runExceptionT y)
    f <*> x = ExceptionT $ liftA2 (<*>) (runExceptionT f) (runExceptionT x)

instance (Alternative m) => Alternative (ExceptionT e m) where
    emtpy = ExceptionT empty
    x <|> y = ExceptionT $ (runExceptionT x) <|> (runExceptionT y)

instance (MonadIO m) => MonadIO (ExceptionT e m) where
    liftIO x = ExpectedT $ fmap Right $ liftIO x

instance MonadTrans (ExceptionT e) where
    lift = liftExceptionT

instance (Monad m) => Monad (ExceptionT e m) where
    return = liftExceptionT . return
    x >>= f = ExceptionT $ do
        { x' <- runExceptionT x
        ; case x' of
            { Left e' -> return $ Left e'
            ; Right a' -> f a'
            }
        }
