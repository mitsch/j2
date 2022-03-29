module Buildin ( Buildin(..)
               , Parameter(..)
               , param
               , ret
               , retM
               , overload
               , mkBuildin
) where

import Data.List ( intercalate )
import Data.List.NonEmpty ( NonEmpty(..), toList, fromList )
import Data.Maybe ( listToMaybe )
import Data.Either ( partitionEithers )

data Buildin a b = Buildin {
    runBuildin :: [a] -> [([Char], a)] -> Either (NonEmpty [Char]) b
}

instance Functor (Buildin a) where
    fmap f a = Buildin $ \oArgs nArgs -> fmap f $ runBuildin a oArgs nArgs

mkBuildin :: b -> Buildin a b
mkBuildin a = Buildin $ \_ _ -> Right a

data Parameter a b = PositionalParameter [Char] (Maybe b) (a -> Either [Char] b)
                   | NamedParameter [Char] (Maybe b) (a -> Either [Char] b)
                   | RegularParameter [Char] (Maybe b) (a -> Either [Char] b)
                   | VariadicPositionalParameter [Char] ([a] -> Either (NonEmpty  [Char]) b)
                   | VariadicNamedParameter [Char] ([([Char], a)] -> Either (NonEmpty [Char]) b)

append :: NonEmpty a -> [a] -> NonEmpty a
append (x:|xs) ys = x :| (xs ++ ys)

param :: Buildin a (b -> c) -> Parameter a b -> Buildin a c
param f (PositionalParameter key dArg ctr)
    = Buildin $ \os ns -> g (h os) (i ns) dArg $ runBuildin f (tail os) (j ns)
    where h = fmap ctr . listToMaybe
          i = lookup key
          j = filter ((key==) . fst)
          g Nothing Nothing Nothing = Left . (msg1:|) . either toList (const [])
          g Nothing Nothing (Just x) = fmap (flip ($) x)
          g Nothing (Just _) _ = Left . (msg2:|) . either toList (const [])
          g (Just _) (Just _) _ = Left . (msg2:|) . either toList (const [])
          g (Just (Left msg)) _ _ = Left . ((msg3 ++ msg):|) . either toList (const [])
          g (Just (Right x)) _ _ = fmap (flip ($) x)
          msg1 = "Parameter " ++ key ++ " without positional nor defaulting argument"
          msg2 = "Parameter " ++ key ++ " has named argument, but is a positional one"
          msg3 = "When evaluating argument for parameter " ++ key ++ ": "
param f (NamedParameter key dArg ctr)
    = Buildin $ \os ns -> g os (h ns) dArg $ runBuildin f [] (i ns)
    where h = fmap ctr . lookup key
          i = filter ((key ==) . fst)
          g [] Nothing Nothing = Left . (msg1:|) . either toList (const [])
          g [] Nothing (Just x) = fmap (flip ($) x)
          g [] (Just (Left msg)) _ = Left . ((msg2 ++ msg):|) . either toList(const [])
          g [] (Just (Right x)) _ = fmap (flip ($) x)
          g (_:_) Nothing _ = Left . (msg3:|) . either toList (const [])
          g (_:_) (Just _) _ = Left . (msg3:|) . either toList (const [])
          msg1 = "Parameter " ++ key ++ " without named or defaulting argument"
          msg2 = "When evaluating argument for parameter " ++ key ++ ": "
          msg3 = "Parameter " ++ key ++ " has positinal argument, but is a named one"
param f (RegularParameter key dArg ctr)
    = Buildin $ \os ns -> g (h os) (i ns) dArg $ runBuildin f (tail os) (j ns)
    where h = fmap ctr . listToMaybe
          i = fmap ctr . lookup key
          j = filter ((key ==) . fst)
          g Nothing Nothing Nothing = Left . (msg1:|) . either toList (const [])
          g Nothing Nothing (Just x) = fmap (flip ($) x)
          g Nothing (Just (Left msg)) _ = Left . ((msg2 ++ msg):|). either toList (const [])
          g Nothing (Just (Right x)) _ = fmap (flip ($) x)
          g (Just _) (Just _) _ = Left . (msg3:|) . either toList (const [])
          g (Just (Left msg)) _ _ = Left . ((msg2 ++ msg):|) . either toList (const [])
          g (Just (Right x)) _ _ = fmap (flip ($) x)
          msg1 = "Parameter " ++ key ++ " without positional nor named nor defaulting argument"
          msg2 = "When evaluating argument for parameter " ++ key ++ ": "
          msg3 = "Parameter " ++ key ++ " with positional and named argument"
param f (VariadicPositionalParameter key ctr)
    = Buildin $ \os ns -> g (ctr os) $ runBuildin f [] ns
    where g (Right x) = fmap (flip ($) x)
          g (Left x) = Left . (append x) . either toList (const [])
param f (VariadicNamedParameter key ctr)
    = Buildin $ \os ns -> g (ctr ns) $ runBuildin f os []
    where g (Right x) = fmap (flip ($) x)
          g (Left x) = Left . (append x) . either toList (const [])

ret :: Buildin a b -> (b -> a) -> Buildin a a
ret a f = Buildin $ \oArgs nArgs -> fmap f $ runBuildin a oArgs nArgs 

retM :: Buildin a b -> (b -> m a) -> Buildin a (m a)
retM a f = Buildin $ \oArgs nArgs -> fmap f $ runBuildin a oArgs nArgs

concatNonEmpty :: NonEmpty (NonEmpty a) -> NonEmpty a
concatNonEmpty (x:|xs) = append x $ concatMap toList xs

overload :: NonEmpty (Buildin a b) -> Buildin a b
overload xs = Buildin
            $ \os ns -> uncurry f
            $ partitionEithers
            $ toList
            $ fmap (\b -> runBuildin b os ns) xs
    where f [] [] = error "Internal error: this should not happen"
          f _ (x:_) = Right x
          f xs [] = Left $ concatNonEmpty $ fromList xs
