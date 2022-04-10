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
import Data.Char ( toUpper
                 , toLower
                 )
import Value ( Value(..)
             , FromValue
             , expectInteger
             , expectFloat
             , expectObject
             , expectList
             , expectString
             , expectBool
             , typeOf
             ) 
import Failure ( MonadFailure, doFail )
import Data.Maybe ( listToMaybe )
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
