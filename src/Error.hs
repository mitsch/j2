{-# LANGUAGE MultiParamTypeClasses #-}

module Error ( Error(..)
             , ErrorTree
             , throwError
             , collectError
             , traceError
    ) where

import Data.List.NonEmpty ( NonEmpty(..) )
import Data.Semigroup

class ErrorTree a where
    throwError :: [Char] -> Int -> [Char] -> a
    collectError :: NonEmpty a -> a
    traceError :: [Char] -> Int -> [Char] -> a -> a


data Error = ErrorLeaf [Char] Int [Char]
           | ErrorTrace [Char] Int [Char] Error
           | ErrorBranch (NonEmpty Error)


instance ErrorTree Error where
    throwError = ErrorLeaf
    collectError = ErrorBranch
    traceError = ErrorTrace

instance Semigroup (Error) where
    (ErrorBranch (a:|as)) <> (ErrorBranch (b:|bs)) = ErrorBranch $ a:|(as ++ b:bs)
    (ErrorBranch (a:|as)) <> b                     = ErrorBranch $ a:|(as ++ [b])
    a                     <> (ErrorBranch (b:|bs)) = ErrorBranch $ a:|(b:bs)
    a                     <> b                     = ErrorBranch $ a:|[b]
