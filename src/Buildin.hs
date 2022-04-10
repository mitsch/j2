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
import Control.Monad ( join )

singleton_ :: a -> NonEmpty a
singleton_ x = x:|[]

relaxedTail :: [a] -> [a]
relaxedTail (_:xs) = xs
relaxedTail [] = []

data Buildin a b = Buildin { positionalArguments :: [a]
                           , namedArguments :: [([Char], a)]
                           , evaluatedValue :: Either (NonEmpty [Char]) b
                           }

mkBuildin :: [a] -> [([Char], a)] -> b -> Buildin a b
mkBuildin ps ns x = Buildin { positionalArguments = ps
                            , namedArguments = ns
                            , evaluatedValue = Right x
                            }

applyResult :: Either (NonEmpty [Char]) (a -> b)
            -> Either (NonEmpty [Char]) a
            -> Either (NonEmpty [Char]) b
applyResult (Left (e1:|es1)) (Left (e2:|es2)) = Left $ e1:|(es1 ++ [e2] ++ es2)
applyResult (Left e) (Right _) = Left e
applyResult (Right _ ) (Left e) = Left e
applyResult (Right f) (Right x) = Right $ f x

applyResult' :: Either (NonEmpty [Char]) a
             -> Either (NonEmpty [Char]) (a -> b)
             -> Either (NonEmpty [Char]) b
applyResult' (Left (e1:|es1)) (Left (e2:|es2)) = Left $ e1:|(es1 ++ [e2] ++ es2)
applyResult' (Left e) (Right _) = Left e
applyResult' (Right _ ) (Left e) = Left e
applyResult' (Right x) (Right f) = Right $ f x

data Parameter a b = PositionalParameter [Char] (Maybe b) (a -> Either [Char] b)
                   | NamedParameter [Char] (Maybe b) (a -> Either [Char] b)
                   | RegularParameter [Char] (Maybe b) (a -> Either [Char] b)
                   | VariadicPositionalParameter [Char] ([a] -> Either (NonEmpty  [Char]) b)
                   | VariadicNamedParameter [Char] ([([Char], a)] -> Either (NonEmpty [Char]) b)

param :: Buildin a (b -> c) -> Parameter a b -> Buildin a c
param tailFunc (PositionalParameter key dArg ctr)
    = Buildin { positionalArguments = relaxedTail ps
              , namedArguments = filter ((key/=) . fst) ns
              , evaluatedValue = applyResult v $ h (i ps) (lookup key ns) dArg
              }
    where ps = positionalArguments tailFunc
          ns = namedArguments tailFunc
          v  = evaluatedValue tailFunc
          i = fmap ctr . listToMaybe
          h Nothing Nothing Nothing = Left $ singleton_ m1
          h Nothing Nothing (Just x) = Right x
          h Nothing (Just _) _ = Left $ singleton_ m2
          h (Just _) (Just _) _ = Left $ singleton_ m2
          h (Just (Left m)) Nothing _ = Left $ singleton_ $ m3 ++ m
          h (Just (Right x)) Nothing _ = Right x
          m1 = "Parameter " ++ key ++ " without positional nor defaulting argument"
          m2 = "Parameter " ++ key ++ " has named argument, but is positional"
          m3 = "When evaluating parameter " ++ key ++ ": "
param tailFunc (NamedParameter key dArg ctr)
    = Buildin { positionalArguments = []
              , namedArguments = filter ((key/=) . fst) ns
              , evaluatedValue = applyResult v $ h (null ps) (i ns) dArg
              }
    where ps = positionalArguments tailFunc
          ns = namedArguments tailFunc
          v = evaluatedValue tailFunc
          i = fmap ctr . lookup key
          h True Nothing Nothing = Left $ singleton_ m1
          h True Nothing (Just x) = Right x
          h True (Just (Left m)) _ = Left $ singleton_ $ m2 ++ m
          h True (Just (Right x)) _ = Right x
          h False _ _ = Left $ singleton_ m3
          m1 = "Parameter " ++ key ++ " without named or defaulting argument"
          m2 = "When evaluating parameter " ++ key ++ ": "
          m3 = "Parameter " ++ key ++ " has positional argument, but is a named one"
param tailFunc (RegularParameter key dArg ctr)
    = Buildin { positionalArguments = relaxedTail ps
              , namedArguments = filter ((key/=) . fst) ns
              , evaluatedValue = applyResult v $ h (i ps) (j ns) dArg
              }
    where ps = positionalArguments tailFunc
          ns = namedArguments tailFunc
          v = evaluatedValue tailFunc
          i = fmap ctr . listToMaybe
          j = fmap ctr . lookup key
          h Nothing Nothing Nothing = Left $ singleton_ m1
          h Nothing Nothing (Just x) = Right x
          h Nothing (Just (Left m)) _ = Left $ singleton_ $ m2 ++ m
          h Nothing (Just (Right x)) _ = Right x
          h (Just (Right x)) Nothing _ = Right x
          h (Just (Left m)) Nothing _ = Left $ singleton_ $ m2 ++ m
          h (Just _) (Just _) _ = Left $ singleton_ m3
          m1 = "Parameter " ++ key ++ " without positional, named or defaulting argument"
          m2 = "When evaluating parameter " ++ key ++ ": "
          m3 = "Parameter " ++ key ++ " has both positional and named arguments"
param tailFunc (VariadicPositionalParameter key ctr)
    = Buildin { positionalArguments = []
              , namedArguments = namedArguments tailFunc
              , evaluatedValue = applyResult (evaluatedValue tailFunc)
                               $ ctr $ positionalArguments tailFunc
              }
param tailFunc (VariadicNamedParameter key ctr)
    = Buildin { positionalArguments = positionalArguments tailFunc
              , namedArguments = []
              , evaluatedValue = applyResult (evaluatedValue tailFunc)
                               $ ctr $ namedArguments tailFunc
              }

-- param :: Buildin a (b -> c) -> Parameter a b -> Buildin a c
-- param f (PositionalParameter key dArg ctr)
--     = Buildin $ \os ns -> g (h os) (i ns) dArg $ runBuildin f (tail os) (j ns)
--     where h = fmap ctr . listToMaybe
--           i = lookup key
--           j = filter ((key==) . fst)
--           g Nothing Nothing Nothing = Left . (msg1:|) . either toList (const [])
--           g Nothing Nothing (Just x) = fmap (flip ($) x)
--           g Nothing (Just _) _ = Left . (msg2:|) . either toList (const [])
--           g (Just _) (Just _) _ = Left . (msg2:|) . either toList (const [])
--           g (Just (Left msg)) _ _ = Left . ((msg3 ++ msg):|) . either toList (const [])
--           g (Just (Right x)) _ _ = fmap (flip ($) x)
--           msg1 = "Parameter " ++ key ++ " without positional nor defaulting argument"
--           msg2 = "Parameter " ++ key ++ " has named argument, but is a positional one"
--           msg3 = "When evaluating argument for parameter " ++ key ++ ": "
-- param f (NamedParameter key dArg ctr)
--     = Buildin $ \os ns -> g os (h ns) dArg $ runBuildin f [] (i ns)
--     where h = fmap ctr . lookup key
--           i = filter ((key ==) . fst)
--           g [] Nothing Nothing = Left . (msg1:|) . either toList (const [])
--           g [] Nothing (Just x) = fmap (flip ($) x)
--           g [] (Just (Left msg)) _ = Left . ((msg2 ++ msg):|) . either toList(const [])
--           g [] (Just (Right x)) _ = fmap (flip ($) x)
--           g (_:_) Nothing _ = Left . (msg3:|) . either toList (const [])
--           g (_:_) (Just _) _ = Left . (msg3:|) . either toList (const [])
--           msg1 = "Parameter " ++ key ++ " without named or defaulting argument"
--           msg2 = "When evaluating argument for parameter " ++ key ++ ": "
--           msg3 = "Parameter " ++ key ++ " has positinal argument, but is a named one"
-- param f (RegularParameter key dArg ctr)
--     = Buildin $ \os ns -> g (h os) (i ns) dArg $ runBuildin f (tail os) (j ns)
--     where h = fmap ctr . listToMaybe
--           i = fmap ctr . lookup key
--           j = filter ((key ==) . fst)
--           g Nothing Nothing Nothing = Left . (msg1:|) . either toList (const [])
--           g Nothing Nothing (Just x) = fmap (flip ($) x)
--           g Nothing (Just (Left msg)) _ = Left . ((msg2 ++ msg):|). either toList (const [])
--           g Nothing (Just (Right x)) _ = fmap (flip ($) x)
--           g (Just _) (Just _) _ = Left . (msg3:|) . either toList (const [])
--           g (Just (Left msg)) _ _ = Left . ((msg2 ++ msg):|) . either toList (const [])
--           g (Just (Right x)) _ _ = fmap (flip ($) x)
--           msg1 = "Parameter " ++ key ++ " without positional nor named nor defaulting argument"
--           msg2 = "When evaluating argument for parameter " ++ key ++ ": "
--           msg3 = "Parameter " ++ key ++ " with positional and named argument"
-- param f (VariadicPositionalParameter key ctr)
--     = Buildin $ \os ns -> g (ctr os) $ runBuildin f [] ns
--     where g (Right x) = fmap (flip ($) x)
--           g (Left x) = Left . (append x) . either toList (const [])
-- param f (VariadicNamedParameter key ctr)
--     = Buildin $ \os ns -> g (ctr ns) $ runBuildin f os []
--     where g (Right x) = fmap (flip ($) x)
--           g (Left x) = Left . (append x) . either toList (const [])

ret :: Buildin a b -> (b -> a) -> Buildin a a
ret x f = Buildin { positionalArguments = []
                  , namedArguments = []
                  , evaluatedValue = applyResult' v
                                   $ h (length $ os) (fmap fst ns)
                  }
        where v = evaluatedValue x
              os = positionalArguments x
              ns = namedArguments x
              h 0 [] = Right f
              h 0 nms = Left $ singleton_
                             $ "Unknown named arguments ("
                             ++ intercalate ", " nms
                             ++ ")"
              h l [] = Left $ singleton_
                            $ "Too many positional arguments ("
                            ++ show l
                            ++ ")"
              h l nms = Left $ singleton_
                             $ "Too many positional arguments ("
                             ++ show l
                             ++ ") and unknown named arguments ("
                             ++ intercalate ", " nms
                             ++ ")"

retM :: Buildin a b -> (b -> Either (NonEmpty [Char]) a) -> Buildin a a
retM x f = Buildin { positionalArguments = []
                   , namedArguments = []
                   , evaluatedValue = join
                                    $ applyResult' v
                                    $ h (length $ os) (fmap fst ns)
                   }
        where v = evaluatedValue x
              os = positionalArguments x
              ns = namedArguments x
              h 0 [] = Right f
              h 0 nms = Left $ singleton_
                             $ "Unknown named arguments ("
                             ++ intercalate ", " nms
                             ++ ")"
              h l [] = Left $ singleton_
                            $ "Too many positional arguments ("
                            ++ show l
                            ++ ")"
              h l nms = Left $ singleton_
                             $ "Too many positional arguments ("
                             ++ show l
                             ++ ") and unknown named arguments ("
                             ++ intercalate ", " nms
                             ++ ")"

overload :: NonEmpty (Buildin a b) -> Buildin a b
overload xs = Buildin { positionalArguments = []
                      , namedArguments = []
                      , evaluatedValue = uncurry f
                                       $ partitionEithers
                                       $ toList
                                       $ fmap evaluatedValue xs
                      }
    where f [] [] = error "Internal error: this should not happen"
          f _ (x:_) = Right x
          f xs [] = Left $ fromList $ concat $ fmap toList xs
