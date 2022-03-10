{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Evaluation ( EvaluationT(..), Evaluation(..), onBadEval ) where

import Control.Monad.Identity ( Identity )

newtype EvaluationT m e a = EvaluationT { runEvaluationT :: m (Either e a) }

type Evaluation e a = EvaluationT Identity e a


instance (Functor m) => Functor (EvaluationT m e) where
    fmap f = EvaluationT . fmap (fmap f) . runEvaluationT

instance (Monad m) => Applicative (EvaluationT m e) where
    pure = EvaluationT . return . pure
    mf <*> mx = EvaluationT $ do
                { f' <- runEvaluationT mf
                ; case f' of
                    { Left e -> return $ Left e
                    ; Right f -> do
                        { x' <- runEvaluationT mx
                        ; case x' of
                            { Left e -> return $ Left e
                            ; Right x -> return $ Right $ f x
                            }
                        }
                    }
                }

instance (Monad m) => Monad (EvaluationT m e) where
    return = EvaluationT . return . return
    mx >>= f = EvaluationT $ do
                { x' <- runEvaluationT mx
                ; either (return . Left) (runEvaluationT . f) $ x'
                }

onBadEval :: Functor m => (e1 -> e2) -> EvaluationT m e1 a -> EvaluationT m e2 a
onBadEval f x = EvaluationT
              $ fmap (either (Left . f) (Right . id))
              $ runEvaluationT x

