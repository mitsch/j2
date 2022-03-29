{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Resolver ( MonadResolver
                , ResolverT(..)
                , resolveName
                , withNames
                , liftResolverT
                ) where

import Data.Maybe (mapMaybe, listToMaybe)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Failure ( MonadFailure, doFail )
import Control.Applicative (Alternative, (<|>), liftA2, empty)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Error ( Error, throwError, collectError, traceError )

class (Monad m) => MonadResolver v m | m -> v where
    resolveName :: [Char] -> m v
    withNames :: [([Char], v)] -> m a -> m a

data ResolverT v m a = ResolverT { runResolverT :: [[([Char], v)]] -> m a }

liftResolverT :: m a -> ResolverT v m a
liftResolverT m = ResolverT (const m)

instance (Monad m) => Monad (ResolverT v m) where
    return = liftResolverT . return
    m >>= k = ResolverT $ \vs -> do
        { a <- runResolverT m vs
        ; runResolverT (k a) vs
        }
    m >> k = ResolverT $ \vs -> runResolverT m vs >> runResolverT k vs

instance MonadTrans (ResolverT v) where
    lift = liftResolverT

instance (Functor m) => Functor (ResolverT v m) where
    fmap f m = ResolverT $ \vs -> fmap f $ runResolverT m vs
    a <$ m = ResolverT $ \vs -> a <$ runResolverT m vs

instance (MonadFailure m) => MonadFailure (ResolverT v m) where
    doFail = liftResolverT . doFail

instance (Applicative m) => Applicative (ResolverT v m) where
    pure = liftResolverT . pure
    f <*> m = ResolverT $ \vs -> runResolverT f vs <*> runResolverT m vs
    m <* n = ResolverT $ \vs -> runResolverT m vs <* runResolverT n vs
    m *> n = ResolverT $ \vs -> runResolverT m vs *> runResolverT n vs
    -- liftA2 f m n = ResolverT $ \vs -> liftA2 f (runResolverT m vs) (runResolverT n vs)

instance (Alternative m) => Alternative (ResolverT v m) where
    empty = liftResolverT empty
    m <|> n = ResolverT $ \vs -> runResolverT m vs <|> runResolverT n vs

instance (MonadIO m) => MonadIO (ResolverT v m) where
    liftIO = liftResolverT . liftIO

instance (Monad m, Error t m) => MonadResolver v (ResolverT v m) where
    resolveName n = ResolverT (f . listToMaybe . mapMaybe (lookup n))
        where f = maybe (throwError $ "Unknown name " ++ show n) return
    withNames ws m = ResolverT $ \vs -> runResolverT m (ws:vs)

instance (Error t m) => Error t (ResolverT v m) where
    throwError = ResolverT . const . throwError
    collectError msg xs = ResolverT
                        $ \ symbs -> collectError msg
                        $ fmap (flip runResolverT symbs) xs
    traceError msg tg x = ResolverT
                        $ \ symbs -> traceError msg tg
                        $ runResolverT x symbs
