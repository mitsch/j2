module Function ( buildin_abs ) where

import Prelude
import Value ( Value(..), Type, expectInteger, expectFloat )
import qualified Prelude ( abs )
import Text.Read ( readMaybe )
import Data.Either ( Either )
import Data.Char ( Char )
import Data.Maybe ( Maybe(..) )
import Data.Semigroup ( Semigroup, sconcat )
import Data.List.NonEmpty ( NonEmpty(..) )
import Control.Applicative ( Applicative, pure, (<*>) )
import Control.Monad.Fail as F

data Evaluation a = Evaluation { runEvaluation :: Either [Char] a }

instance Functor Evaluation where
    fmap f x = Evaluation $ fmap f $ runEvaluation x

instance Applicative Evaluation where
    pure = Evaluation . pure
    a <*> b = Evaluation $ (runEvaluation a) <*> (runEvaluation b)

instance Monad Evaluation where
    return = Evaluation . return
    x >>= f = either (Evaluation . Left) f $ runEvaluation x

instance F.MonadFail Evaluation where
    fail = Evaluation . Left

data CurriedFunction a = CurriedFunction {
    runCurriedFunction :: a -> [Value] -> [([Char], Value)] -> Evaluation Value
}

newtype Function = Function {
    runFunction :: [Value] -> [([Char], Value)] -> Evaluation Value
}

invalidate :: [Char] -> Evaluation a
invalidate = Evaluation . Left

overload :: NonEmpty Function -> Function
overload fs = Function $ \ovs kvs -> g $ fmap (\f -> (runFunction f) ovs kvs) fs
    where g = Evaluation . sconcat . fmap runEvaluation

call :: a -> CurriedFunction a -> Function
call f e = Function $ \ovs kvs -> (runCurriedFunction e) f ovs kvs

ret :: (a -> Value) -> CurriedFunction a
ret f = CurriedFunction $ \x _ _ -> pure $ f x

ret' :: (a -> Evaluation Value) -> CurriedFunction a
ret' f = CurriedFunction $ \x _ _ -> f x

-- Adds parameter at new first to a function
-- k (:: [Char])                  key name of parameter
-- dv (:: Maybe a)                default value of parameter
-- ctr (:: Value -> Evaluation a) constructor of parameter from arbirary value
-- rf (:: Function b)             remaining part of function
param :: [Char] -> Maybe a -> (Value -> Evaluation a) -> CurriedFunction b -> CurriedFunction (a -> b)
param k dv ctr rf = CurriedFunction $ \f ovs kvs -> case ovs of
    { [] -> maybe (maybe msg1 h1 dv) h2 $ lookup k kvs where
        msg1 = invalidate $ "Parameter \"" ++ k ++ "\" has no positional nor named nor default argument!"
        h1 x = runCurriedFunction rf (f x) [] kvs
        h2 v = ctr v >>= \x -> runCurriedFunction rf (f x) [] kvs
    ; (o:os) -> maybe h3 (const msg3) $ lookup k kvs where
        h3 = ctr o >>= \x -> runCurriedFunction rf (f x) os kvs
        msg3 = invalidate $ "Parameter \"" ++ k ++ "\" has positional and named argument!"
    }

buildin_abs = overload $ doInt :| [doFloat] where
    doInt = call Prelude.abs $ param "x" Nothing expectInteger $ ret IntegerVal
    doFloat = call Prelude.abs $ param "x" Nothing expectFloat $ ret FloatVal

-- -- 
-- -- buildin_attr = callBuildin lookup
-- --              $ param "obj" Nothing expectObject
-- --              $ param "str" Nothing expectString
-- --              $ ret' (maybe (Left emsg) Right)
-- --     where emsg = "Cannot find attribute in object"
-- -- 
-- -- _batch :: [a] -> Int -> Maybe a -> [[a]]
-- -- _batch xs n d = f xs
-- --     where f [] = []
-- --           f xs = (take n $ take n xs ++ (maybe [] repeat $ d)):(f $ drop n xs)
-- -- 
-- -- buildin_batch = callBuildin _batch
-- --               $ param "value" Nothing ()
-- --               $ param "linecount" Nothing expectInteger
-- --               $ param "fill_with" (Just Nothing) (Just)
-- --               $ ret ListVal
-- -- 
-- -- buildin_capitalize = callBuildin f
-- --                    $ param "s" Nothing expectString
-- --                    $ ret StringVal
-- --     where f [] = []
-- --           f (x:xs) = (toUpper x):xs
-- -- 
-- -- buildin_center = callBuildin f
-- --                $ param "value" Nothing expectString
-- --                $ param "width" (Just 80) expectInteger
-- --                $ ret StringVal
-- --     where f x n = let a = flip replicate ' ' $ div (max 0 (n - length x)) 2
-- --                   in a ++ x ++ a
-- -- 
-- -- buildin_default = callBuildin f
-- --                 $ param "value" Nothing id
-- --                 $ param "default_value" (Just $ StringVal "") id
-- --                 $ param "boolean" (Just False) expectBool
-- --                 $ ret id
-- --     where f x d True = case testBool x of { True -> x; False -> d}
-- --           f x d False = case isNone x of { True -> d; False -> x}
-- -- 
-- -- buildin_dictsort = callBuildin f
-- --                  $ param "value" Nothing expectDictionary
-- --                  $ param "case_sensitive" (Just False) expectBool
-- --                  $ param "by" (Just ()) (const $ pure ())
-- --                  $ param "reverse" (Just False) expectBool
-- --                  $ ret DictionaryVal
-- --     where f :: [(Value, Value)] -> Bool -> () -> Bool -> [(Value, Value)]
-- --           f xs c _ r = xs
-- -- 
-- -- buildin_escape = callBuildin (concatMap f)
-- --                $ param "value" Nothing (\v -> expectString v <|> g v)
-- --                $ ret StringVal
-- --     where f '&' = "&amp;"
-- --           f '<' = "&lt;"
-- --           f '>' = "&gt;"
-- --           f '\'' = "&#39;"
-- --           f '\"' = "&quot;"
-- --           f x = [x]
-- --           g v = do { o <- expectObject v
-- --                    ; w <- case lookup "__html__" o of
-- --                             { Nothing -> fail $ "Cannot find a method \"__html__\""
-- --                             ; Just x -> return x
-- --                             }
-- --                    ; h <- expectFunction w
-- --                    ; y <- h [] []
-- --                    ; expectString y
-- --                    }
-- -- 
-- -- buildin_filesizeformat = callBuildin f
-- --                        $ param "value" Nothing (\x -> expectInteger x <|> expectString x >>= g)
-- --                        $ param "binary" (Just False) expectBool
-- --                        $ ret StringVal
-- --     where f n False = maybe (show n ++ "B") id
-- --                     $ listToMaybe
-- --                     $ reverse
-- --                     $ catMaybes
-- --                     $ fmap (\(s,p) -> case n >= (10^p) of
-- --                         { True -> (show $ div n $ 10^p) ++ p
-- --                         ; False -> Nothing
-- --                         })
-- --                     $ zip ["KB", "MB", "GB", "TB", "PB", "EB", "ZB"]
-- --                     $ iterate 3 (3+)
-- --           f n True = maybe (show n ++ "B") id
-- --                    $ listToMaybe
-- --                    $ reverse
-- --                    $ catMaybes
-- --                    $ fmap (\(s,p) -> case n >= (2^p) of
-- --                         { True -> (show $ div n $ 2^p) ++ p
-- --                         ; False -> Nothing
-- --                         })
-- --                    $ zip ["KiB", "MiB", "GiB", "TiB", "PiB", "EiB", "ZiB"]
-- --                    $ iterate 10 (10+)
-- --           g :: [Char] -> Either [Char] Integer
-- --           g = maybe (Left $ "Cannot read Integer") (Right . fst)
-- --             . listToMaybe
-- --             . reads
-- -- 
-- -- buildin_first = callBuildin listToMaybe
-- --               $ param "seq" Nothing expectList
-- --               $ ret' (either (Left "Empty List") Right)
-- -- 
-- -- buildin_float = callBuildin f
-- --               $ param "value" Nothing g
-- --               $ param "default" (Just (0%0)) expectDecimal
-- --               $ ret DecimalVal
-- --     where g :: Value -> Either [Char] (Maybe Rational)
-- --           g v = asDecimal v <|> ((%1) <$> asInteger v) <|> (asString v >>= readMaybe)
-- --           f v d = maybe d Right v
-- -- 
-- -- buildin_forceescape = callBuildin (either (concatMap f) id)
-- --                     $ param "value" Nothing (\x -> (Left <$> expectString x) <|>
-- --                                                    (Right <$> g x))
-- --                     $ ret StringVal
-- --     where g v = do { obj <- expectObject v
-- --                    ; mbr <- case lookup "__html__" obj of
-- --                     { Nothing -> fail "Cannot find method \"__html__\""
-- --                     ; Just x -> return x
-- --                     }
-- --                    ; fun <- expectFunction mbr
-- --                    ; res <- fun [] []
-- --                    ; expectString res
-- --                    }
-- --           f '&' = "&amp;"
-- --           f '<' = "&lt;"
-- --           f '>' = "&gt;"
-- --           f '\'' = "&#39;"
-- --           f '\"' = "&quot;"
-- --           f x = [x]
-- -- 
-- -- -- TODO implementation of buildin_format
-- -- 
-- -- buildin_groupby = callBuilding f
-- --                 $ param "value" Nothing g
-- --                 $ param "attribute" Nothing ()
-- --                 $ param "default" (Just Nothing) i
-- --                 $ ret j
-- --     where 
-- -- 
-- -- _indent :: [Char] -> Int -> Bool -> Bool -> [Char]
-- -- _indent s w True True = unlines $ fmap (f . g) $ lines s
-- --     where g = dropWhile isSpaces
-- --           f = ((repeat w ' ')++)
-- -- _indent s w True False = unlines $ fmap () $ lines s
-- --     where f 
-- -- _indent s w False b = f $ lines s
-- --     where f (l:ls) = unlines $ (l:) $ fmap (g b) ls
-- --           f [] = []
-- --           g True x = ((repeat w ' ')++) $ dropWhile isSpaces x
-- --           g False x = case all isSpace x of
-- --                     { True -> x
-- --                     ; False -> ((repeat w ' ')++) $ dropWhile isSpace x
-- --                     }
-- -- 
-- -- builtin_indent xs = foo ["s", "width", "first", "blank"] $ do
-- --     { text <- pop >>= expectString
-- --     ; width <- pop >>= expectInteger <|> (expectString >>= toInteger) <?> 80
-- --     ; first <- pop >>= expectBoolean <?> False
-- --     ; blank <- pop >>= expectBoolean <?> False
-- --     ; return _indent text width first blank
-- --     }
-- -- 
-- -- builtin_indent = _indent
-- --            `foo` (bar "s" $ expectString)
-- --            `foo` (bar "width" $ expectInteger <|> (expectString >>= toInteger) <?> 80)
-- --            `foo` (bar "first" $ expectBoolean <?> False)
-- --            `foo` (bar "blank" $ expectBoolean < False)
-- -- 
-- -- 
-- -- 
-- -- 
