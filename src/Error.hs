module Error ( Error(..)
             , ErrorTree
             , throwError
             , collectError
             , traceError
    ) where

class ErrorTree l a | a -> l where
    throwError :: [Char] -> l -> a
    collectError :: a -> a -> a
    traceError :: [Char] -> l -> a -> a


data Error l = ErrorReason [Char] l
             | ErrorTrace [Char] l (Error l)
             | ErrorBranch (Error l) (Error l)


instance ErrorTree l (Error l) where
    throwError = ErrorLeaf
    collectError = ErrorBranch
    traceError = ErrorTrace

instance Semigroup (Error) where
    (<>) = ErrorBranch
