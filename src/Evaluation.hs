module Evaluation ( Evaluation(..) ) where

import Control.Monad.Fail ( MonadFail, fail )

data Evaluation a = Evaluation { runEvaluation :: Either [Char] a }

instance Functor (Evaluation) where
    fmap f = Evaluation . fmap f . runEvaluation

instance Applicative (Evaluation) where
    pure = Evaluation . Right
    f <*> x = Evaluation $ (runEvaluation f) <*> (runEvaluation x)

instance Monad (Evaluation) where
    return = Evaluation . Right
    x >>= f = either (Evaluation . Left) f $ runEvaluation x

instance MonadFail (Evaluation) where
    fail = Evaluation . Left

