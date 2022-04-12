{-# LANGUAGE FlexibleInstances #-}

module BuildinFilters ( buildin_abs
                      , buildin_attr
                      , buildin_batch
                      , buildin_capitalize
                      , buildin_center
                      , buildin_default
                      , buildin_escape
                      , buildin_filesizeformat
                      , buildin_first
                      , buildin_float
                      , buildin_groupby
) where

import Buildin ( Buildin(..)
               , Parameter(..)
               , param
               , ret
               , retM
               , overload
               , mkBuildin
               )
import Data.List.NonEmpty ( NonEmpty(..), fromList )
import Data.List ( groupBy )
import Data.Char ( toUpper
                 , toLower
                 , isDigit
                 , isAlpha
                 )
import Data.Ord ( compare )
import Value ( Value(..)
             , FromValue
             , expectInteger
             , expectFloat
             , expectObject
             , expectList
             , expectString
             , expectBool
             , typeOf
             , listVal
             , stringVal
             ) 
import Failure ( MonadFailure, doFail )
import Data.Maybe ( listToMaybe, catMaybes )
import Data.Function ( on )
import GHC.Float.RealFracMethods ( truncateFloatInteger )


fromOptional :: Value -> Maybe Value
fromOptional NoneVal = Nothing
fromOptional x = Just x

toBool :: Value -> Bool
toBool NoneVal = False
toBool (BoolVal x) = x
toBool (IntegerVal x) = x /= 0
toBool (FloatVal x) = x /= 0
toBool (StringVal x) = not $ null x
toBool (ListVal x) = not $ null x
toBool (DictionaryVal x) = not $ null x
toBool (ObjectVal x) = not $ null x
toBool _ = False

_singleton :: a -> NonEmpty a
_singleton x = x:|[]


buildin_abs :: [Value] -> [([Char], Value)] -> Buildin Value Value
buildin_abs os ns = overload $ fromList
    [ mkBuildin os ns Prelude.abs
        `param` (RegularParameter "x" Nothing expectInteger)
        `ret`   IntegerVal
    , mkBuildin os ns Prelude.abs
        `param` (RegularParameter "x" Nothing expectFloat)
        `ret`   FloatVal
    ]

buildin_attr os ns = mkBuildin os ns (\ns x -> (lookup x ns, x))
    `param` (RegularParameter "obj" Nothing expectObject)
    `param` (RegularParameter "name" Nothing expectString)
    `retM`  f
    where f :: (Maybe Value, [Char]) -> Either (NonEmpty [Char]) Value
          f (Nothing, x) = Left
                         $ _singleton
                         $ "Could not find attribute with name \"" ++ x ++ "\""
          f (Just x, _) = pure x

buildin_batch os ns = mkBuildin os ns (\a b c -> (ListVal . fmap ListVal) $ f a b c)
    `param` (RegularParameter "value" Nothing expectList)
    `param` (RegularParameter "linecount" Nothing g)
    `param` (RegularParameter "fill_with" (Just Nothing) (pure . fromOptional))
    `ret`   id
    where f [] n v = []
          f xs n v = let (a,b) = splitAt n xs
                     in (take n $ a ++ (maybe [] repeat v)) : f b n v
          g x = do { y <- expectInteger x
                   ; if y > 0 then return $ fromIntegral y
                              else doFail $ "Parameter linecount must be positive but is " ++ show y ++ "!"
                   }

buildin_capitalize os ns = mkBuildin os ns f
    `param` (RegularParameter "s" Nothing expectString)
    `ret` StringVal
    where f :: [Char] -> [Char]
          f [] = []
          f (x:xs) = toUpper x : fmap toLower xs

buildin_center os ns = mkBuildin os ns f
    `param` (RegularParameter "value" Nothing expectString)
    `param` (RegularParameter "width" (Just 80) (fmap fromIntegral . expectInteger))
    `ret` StringVal
    where f :: [Char] -> Int -> [Char]
          f xs n = let m = (max 0 (n - length xs)) `div` 2
                       p = max 0 $ n - length xs - m
                   in (replicate m ' ') ++ xs ++ (replicate p ' ')

buildin_default os ns = mkBuildin os ns f
    `param` (RegularParameter "value" Nothing pure)
    `param` (RegularParameter "default" (Just Nothing) (pure . Just))
    `param` (RegularParameter "boolean" (Just False) expectBool)
    `ret` id
    where f :: Value -> Maybe Value -> Bool -> Value
          f NoneVal d False = maybe (StringVal "") id d
          f x       _ False = x
          f x       d True  = case toBool x of
                                { True -> x
                                ; False -> maybe (StringVal "") id d
                                }

-- buildin_dictsort = mkBuildin g
--     `param` (RegularParameter "value" Nothing expectDictionary)
--     `param` (RegularParameter "case_sensitive" (Just False) expectBool)
--     `param` (RegularParameter "by" (Just ByKey) f)
--     `param` (RegularParameter "reverse" (Just False) expectBool)
--     `ret`   ()
--     where By = ByKey | ByValue
--           f x = expectString x >>= \y -> case y of
--                 { "key" -> return ByKey
--                 ; "value" -> return ByValue
--                 ; _ -> doFail $ "invalid value for parameter \"by\": should be either \"key\" or \"value\" but is " ++ show y
--                 }
--           g xs c ByKey 


buildin_escape os ns = mkBuildin os ns (either id f)
    `param` (RegularParameter "s" Nothing g)
    `ret`   StringVal
    where g :: (Monad m, MonadFailure m) => Value -> m (Either [Char] [Char])
          g (StringVal x) = return $ Right x
          g (ObjectVal xs) = case lookup "__html__" xs of
                { Just y -> doFail "Deriving string from \"__html__\" is not supported so far"
                ; Nothing -> doFail "Got object, but missing method \"__html__\""
                }
          g x = doFail $ "Expected string or object with method \"__html__\", but got " ++ show (typeOf x)
          f xs = flip concatMap xs $ \x -> case x of
                    { ' '  -> "&nbsp;"
                    ; '&'  -> "&amp;"
                    ; '<'  -> "&lt;"
                    ; '>'  -> "&gt;"
                    ; '\'' -> "&apos;"
                    ; '\"' -> "&quot;"
                    ; '¢'  -> "&cent;"
                    ; '£'  -> "&pound;"
                    ; '¥'  -> "&yen;"
                    ; '€'  -> "&euro;"
                    ; '©'  -> "&copy;"
                    ; '®'  -> "&reg;"
                    ; c    -> [c]
                    }

buildin_filesizeformat os ns
    =       mkBuildin os ns f
    `param` (RegularParameter "value" Nothing g)
    `param` (RegularParameter "binary" (Just False) expectBool)
    `ret`   StringVal
    where f x b = maybe (show x ++ "B") (\(s,l) -> show (x `div` l) ++ s)
                $ listToMaybe
                $ reverse
                $ takeWhile (\(s, l) -> x >= l)
                $ case b of
                { True -> zip binarySuffixes $ iterate (1024*) 1024
                ; False -> zip decimalSuffixes $ iterate (1000*) 1000
                }
          g :: Value -> Either [Char] Integer
          g (StringVal x) = case reads x of
                          { [(x,[])] -> pure x
                          ; _ -> Left $ "Parameter \"value\" has invalid argument; it should be an integer but is \"" ++ x ++ "\""
                          }
          g (FloatVal x) = pure $ truncateFloatInteger x
          g (IntegerVal x) = pure x
          decimalSuffixes = ["kB", "MB", "GB", "TB", "PB", "EB", "ZB", "YB"]
          binarySuffixes = ["kiB", "MiB", "GiB", "TiB", "PiB", "EiB", "ZiB", "YiB"]

buildin_first os ns
    =       mkBuildin os ns listToMaybe
    `param` (RegularParameter "seq" Nothing expectList)
    `ret`   (maybe NoneVal id)

buildin_float os ns = mkBuildin os ns f
                    `param` (RegularParameter "value" Nothing pure)
                    `param` (RegularParameter "default" (Just 0.0) expectFloat)
                    `ret` FloatVal
    where f (FloatVal x) _ = x
          f (IntegerVal x) _ = fromInteger x
          f (StringVal x) d = case reads x of { [(y,[])] -> y; _ -> d}
          f _ x = x

-- TODO how to call on object???
-- buildin_forceescape os ns
--    = mkBuildin os ns ()
--    `param` (RegularParameter "value" Nothing 
--


--buildin_format os ns = mkBuildin os ns f
--    `param` (RegularParameter "value" Nothing expectString)
--    `param` (VariadicPositionalParameter "args" (sequenceA . fmap pure))
--    `param` (VariadicNamedParameter "kwargs" (sequenceA . fmap pure))
--    `ret` 
--    where f s as kas = 
--


splitOn :: Char -> [Char] -> [[Char]]
splitOn d xs = g xs
    where g ys = let (pre, post) = break ('.'==) ys
                 in pre:(g $ drop 1 post)

liftValidAttributeName :: [Char] -> Either [Char] [Char]
liftValidAttributeName [] = Left "empty string for attribute name"
liftValidAttributeName (x:xs) = case isAlpha x && all (\x -> isAlpha x || isDigit x) xs of
                              { True -> Right (x:xs)
                              ; False -> Left "occurrence of non-alpha and non-digit"
                              }

buildin_groupby os ns = mkBuildin os ns f
    `param` (RegularParameter "value" Nothing expectList)
    `param` (RegularParameter "attribute" Nothing j)
    `param` (RegularParameter "default" (Just NoneVal) pure)
    `ret`   (listVal . fmap (\(k,vs) -> listVal $ [k, listVal vs]))
    where f :: [Value] -> Either [[Char]] Int -> Value -> [(Value, [Value])]
          f xs s dVal = catMaybes
                      $ fmap i
                      $ groupBy ((==) `on` snd)
                      $ zip xs
                      $ fmap (g s dVal) xs
          g (Left ks) d x = h d ks x
          g (Right i) d x = maybe d (maybe d id . listToMaybe . drop i)
                          $ expectList x
          h :: Value -> [[Char]] -> Value -> Value
          h d [] x = x
          h d (k:ks) (ObjectVal as) = maybe d (h d ks) $ lookup k as
          h d (k:ks) (DictionaryVal es) = maybe d (h d ks) $ lookup (stringVal k) es
          h d _ _ = d
          i [] = Nothing
          i ((v,k):xs) = Just (k, v:(fmap fst xs))
          j :: Value -> Either [Char] (Either [[Char]] Int)
          j (IntegerVal x) = case x < 0 of
                           { True -> Left "Invalid argument, expected to be non-negative but is negative"
                           ; False -> Right $ Right $ fromInteger x
                           }
          j (StringVal x) = either (\e -> Left $ ("error in attribute name: " ++ e)) (Right . Left)
                          $ sequenceA
                          $ fmap liftValidAttributeName
                          $ splitOn '.' x
          j x = Left $ "Expected integer or string but got " ++ (show $ typeOf x)
