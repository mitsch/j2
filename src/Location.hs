module Location ( Location(..) ) where

data Location = BuildinLocation [Char]
              | FileLocation [Char] Int

instance Show (Location) where
    show (BuildinLocation n) = "builtin<" ++ n ++ ">"
    show (FileLocation p l) = p ++ ":" ++ (show l)

instance Eq (Location) where
    (==) (BuildinLocation a) (BuildinLocation b) = (==) a b
    (==) (FileLocation a b) (FileLocation c d) = a == c && b == d
    (==) _ _ = False

