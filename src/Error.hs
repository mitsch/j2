{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Error ( Error
             , throwError
             , collectError
             , traceError
             , ErrorTree(..)
             , ExceptionT(..)
             , Exception(..)
    ) where

import Data.List.NonEmpty ( NonEmpty(..) )
import Data.Semigroup
import Control.Monad.Identity ( Identity )
import Data.Bifunctor ( bimap )
import qualified Control.Monad.Fail as F
import Control.Monad.IO.Class

class Error t m | m -> t where
    throwError :: [Char] -> m a
    collectError :: NonEmpty (m a) -> m a
    traceError :: [Char] -> t -> m a -> m a


data ErrorTree t = ErrorLeaf [Char]
                 | ErrorTrace [Char] t (ErrorTree t)
                 | ErrorBranch (NonEmpty (ErrorTree t))

data ExceptionT t m a = ExceptionT {
    runException :: m (Either (ErrorTree t) a)
}

type Exception t a = ExceptionT t Identity a

instance (Functor m) => Functor (ExceptionT t m ) where
    fmap f = ExceptionT . fmap (fmap f) . runException

instance (Applicative m) => Applicative (ExceptionT t m) where
    pure = ExceptionT . pure . pure
    f <*> x = ExceptionT $ (<*>) <$> (runException f) <*> (runException x)

instance (Monad m) => Monad (ExceptionT t m) where
    return = ExceptionT . return . return
    x >>= f = ExceptionT $ runException x >>= either (return . Left) (runException . f)

instance (Functor m, Applicative m) => Error t (ExceptionT t m) where
    throwError msg = ExceptionT $ pure $ Left $ ErrorLeaf msg
    collectError errs = ExceptionT
                      $ fmap (either Right (Left . ErrorBranch) . traverse (either Right Left))
                      $ traverse runException errs
    traceError msg tg err = ExceptionT
                          $ fmap (bimap (ErrorTrace msg tg) id)
                          $ runException err

instance (F.MonadFail m) => F.MonadFail (ExceptionT t m) where
    fail = ExceptionT . fail

instance (MonadIO m, Functor m) => MonadIO (ExceptionT t m) where
    liftIO x = ExceptionT $ fmap Right $ liftIO x
