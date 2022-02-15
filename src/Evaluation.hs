module Evaluation ( Evaluation(..) ) where

import Control.Monad.Fail ( MonadFail, fail )
import Error ( Error, ErrorTree, throwError, collectError, traceError )
import Data.List.NonEmpty ( NonEmpty(..) )

data Evaluation a = Evaluation { runEvaluation :: Either Error a }

instance Functor (Evaluation) where
    fmap f = Evaluation . fmap f . runEvaluation

instance Applicative (Evaluation) where
    pure = Evaluation . Right
    f <*> x = Evaluation $ (runEvaluation f) <*> (runEvaluation x)

instance Monad (Evaluation) where
    return = Evaluation . Right
    x >>= f = either (Evaluation . Left) f $ runEvaluation x

instance ErrorTree (Evaluation a) where
    throwError msg ln pth = Evaluation $ Left $ throwError msg ln pth
    collectError = Evaluation
                 . either Right (Left . collectError)
                 . traverse (either Right Left)
                 . fmap runEvaluation
    traceError msg ln pth err = Evaluation
                              $ either (Left . traceError msg ln pth) Right
                              $ runEvaluation err


