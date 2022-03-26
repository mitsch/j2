{-# LANGUAGE FlexibleInstances #-}

module Function ( Function(..)
                , callFunction
) where

import Prelude
import qualified Prelude ( abs )
import Text.Read ( readMaybe )
import Data.Either ( Either, partitionEithers )
import Data.Char ( Char )
import Data.Maybe ( Maybe(..) )
import Data.Semigroup ( Semigroup, sconcat, (<>) )
import Data.List.NonEmpty ( NonEmpty(..) )
import Data.List ( intersect )
import Control.Monad.Fail ( MonadFail )
import Error ( throwError )
import Resolver ( MonadResolver, withNames )
-- import Value ( ToValue, listVal )


data (MonadResolver a m) => Function m a = Function
    { ordinalParams :: [[Char]]
    , ordinalDefParams :: [([Char], a)]
    , namedOrOrdinalParams :: [[Char]]
    , namedOrOrdinalDefParams :: [([Char], a)]
    , variadicOrdinalParams :: Maybe [Char]
    , namedParams :: [[Char]]
    , namedDefParams :: [([Char], a)]
    , variadicNamedParams :: Maybe [Char]
    , body :: m a
    }

bindOrdinalParams :: [[Char]] -> [a] -> Either [[Char]] [([Char], a)]
bindOrdinalParams params args = case miss of
    { [] -> Right $ zip params args
    ; xs -> Left $ fmap g xs
    }
    where (hit, miss) = splitAt (length args) params
          g x = "missing positional argument for parameter \"" ++ x ++ "\""

bindOrdinalDefParams :: [([Char], a)] -> [a] -> [([Char], a)]
bindOrdinalDefParams params args = (zip (fmap fst hit) args) ++ miss
    where (hit, miss) = splitAt (length args) params

bindNamedOrdinalParams :: [[Char]] -> [([Char], a)] -> [a] -> Either [[Char]] [([Char], a)]
bindNamedOrdinalParams params namArgs posArgs = case unsets of
    { [] -> Right $ (zip hit posArgs) ++ sets
    ; xs -> Left $ fmap f xs
    }
    where (hit, miss) = splitAt (length posArgs) params
          (unsets, sets) = partitionEithers $ fmap g miss
          g x = maybe (Left x) (Right . (,) x) $ lookup x namArgs
          f x = "missing positional or named argument for parameter \"" ++ x ++ "\""

bindNamedOrdinalDefParams :: [([Char], a)] -> [([Char], a)] -> [a] -> [([Char], a)]
bindNamedOrdinalDefParams params namArgs posArgs = zip keys posArgs ++ miss
    where keys = fmap fst params
          miss = fmap f $ drop (length posArgs) params
          f (k,d) = maybe (k,d) ((,) k) $ lookup k namArgs

bindVariadicOrdinalParams :: Maybe [Char] -> [a] -> Either [[Char]] [([Char], a)]
-- TODO need some abstraction from Value.ToValue
-- bindVariadicOrdinalParams (Just n) posArgs = Right [(n, listVal $ posArgs)]
bindVariadicOrdinalParams (Just n) posArgs = Right []
bindVariadicOrdinalParams Nothing [] = Right []
bindVariadicOrdinalParams Nothing posArgs = Left ["too many positional arguments"]

bindNamedParams :: [[Char]] -> [([Char], a)] -> Either [[Char]] [([Char], a)]
bindNamedParams params namArgs = case unsets of
    { [] -> Right sets
    ; xs -> Left $ fmap g xs
    }
    where (sets, unsets) = partitionEithers $ fmap f params
          f k = maybe (Right k) (Left . (,) k) $ lookup k namArgs
          g x = "parameter \"" ++ x ++ "\" is not set"

bindNamedDefParams :: [([Char], a)] -> [([Char], a)] -> [([Char], a)]
bindNamedDefParams params namArgs = fmap f params
    where f (k, v) = maybe (k, v) ((,) k) $ lookup k namArgs

bindVariadicNamedParams :: Maybe [Char] -> [([Char], a)] -> [[Char]] -> Either [[Char]] [([Char], a)]
-- TODO need some abstraction away from Value.ToValue
-- bindVariadicNamedParams (Just n) namArgs keys = Right [(n, objectVal entries)]
bindVariadicNamedParams (Just n) namArgs keys = Right []
    where entries = filter (flip notElem keys . fst) namArgs
bindVariadicNamedParams Nothing [] _ = Right []
bindVariadicNamedParams Nothing xs keys = Left $ fmap (f . fst) $ filter g xs
    where g = flip notElem keys . fst
          f k = "unknown parameter name \"" ++ k ++ "\""

testOverlappingKeys :: [a] -> [([Char], a)] -> [[Char]] -> Either [[Char]] [b]
testOverlappingKeys ordinalArgs namedArgs keys = case overlappingKeys of
    { [] -> Right []
    ; xs -> Left $ fmap f xs
    }
    where overlappingKeys = intersect (fmap fst namedArgs)
                          $ take (length ordinalArgs)
                          $ keys
          f x = "Parameter \"" ++ x ++ "\" has positional and named argument"

callFunction :: (MonadResolver a m)
             => Function m a -> [a] -> [([Char], a)] -> Either [[Char]] (m a)
callFunction f ordinalArgs namedArgs = case errors of
    { [] -> Right $ withNames (concat bounds) $ body f
    ; es -> Left $ concat es
    }
    where (errors, bounds) = partitionEithers
            [ bindOrdinalParams (ordinalParams f) ordinalArgs
            , Right
                $ bindOrdinalDefParams (ordinalDefParams f)
                $ drop (length $ ordinalParams f)
                $ ordinalArgs
            , bindNamedOrdinalParams (namedOrOrdinalParams f) namedArgs
                $ drop (length $ ordinalParams f)
                $ drop (length $ ordinalDefParams f)
                $ ordinalArgs
            , Right
                $ bindNamedOrdinalDefParams (namedOrOrdinalDefParams f) namedArgs
                $ drop (length $ ordinalParams f)
                $ drop (length $ ordinalDefParams f)
                $ drop (length $ namedOrOrdinalParams f)
                $ ordinalArgs
            , testOverlappingKeys ordinalArgs namedArgs $ concat
                [ ordinalParams f
                , fmap fst $ ordinalDefParams f
                , namedOrOrdinalParams f
                , fmap fst $ namedOrOrdinalDefParams f
                ]
            , bindVariadicOrdinalParams (variadicOrdinalParams f)
                $ drop (length $ ordinalParams f)
                $ drop (length $ ordinalDefParams f)
                $ drop (length $ namedOrOrdinalParams f)
                $ drop (length $ namedOrOrdinalDefParams f)
                $ ordinalArgs
            , bindNamedParams (namedParams f) namedArgs
            , Right $ bindNamedDefParams (namedDefParams f) namedArgs
            , bindVariadicNamedParams (variadicNamedParams f) namedArgs
                $ concat [ namedOrOrdinalParams f
                         , fmap fst $ namedOrOrdinalDefParams f
                         , namedParams f
                         , fmap fst $ namedDefParams f
                         ]
            ]



-- buildin_abs = overload (doInt :| [doFloat]) where
--    doInt = invokeFn Prelude.abs
--          $ param "x" Nothing (\v -> maybe (throwError $ "not an integer" "builtin abs") pure $ asInteger v)
--          $ pureFn IntegerVal
--    doFloat = invokeFn Prelude.abs
--            $ param "x" Nothing (maybe (throwError "not a float" "builtin abs") pure . asFloat)
--            $ pureFn FloatVal
-- 
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
