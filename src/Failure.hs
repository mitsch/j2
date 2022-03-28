{-# LANGUAGE FlexibleInstances #-}

module Failure ( MonadFailure, doFail ) where


class (Monad m) => MonadFailure m where
    doFail :: [Char] -> m a


instance MonadFailure ( Maybe ) where
    doFail = const Nothing

instance MonadFailure ( Either [Char] ) where
    doFail = Left

instance MonadFailure ( [] ) where
    doFail = const []
