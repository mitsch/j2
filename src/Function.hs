module Function ( abs
                , attr
                , batch
                , capitalize
                ) where

import Value ( Value, Type, toType )
import Control.Monad.IO
import qualified Prelude ( abs )
import Text.Read ( readMaybe )


data Buildin a = Buildin
    {
        runBuildin :: a -> [Value] -> [([Char], Value)] -> Either [Char] Value
    }

callBuildin :: a -> Buildin a -> ([Value] -> [([Char], Value)] -> Either [Char] Value)
callBuildin f b = (runBuildin b) f

overloadBuildin :: ([Value] -> [([Char], Value)] -> Either [Char] Value)
                -> ([Value] -> [([Char], Value)] -> Either [Char] Value)
                -> ([Value] -> [([Char], Value)] -> Either [Char] Value)
overloadBuildin a b = \os ns -> (a os ns) <|> (b os ns)

ret :: (a -> Value) -> Buildin a
ret f = Buildin $ \x _ _ -> pure $ f x

ret' :: (a -> Either [Char] Value) -> Buildin a
ret' f = Buildin $ \x _ _ -> f a

param :: [Char] -> Maybe a -> (Value -> Either [Char] a) -> Buildin b -> Buildin (a -> b)
param n d f r = Buildin $ \g oss ns -> case oss of
    { [] -> case lookup n ns of
        { Nothing -> case d of
            { Nothing -> Left $ "Parameter \"" ++ n ++ "\" has no positional, named or default argument."
            ; Just x -> (runBuildin r) (g x) [] ns
            }
        ; Just x -> (fmap g (f x)) >>= \y -> (runBuilding r) y [] ns
        }
    ; (o:os) -> case lookup n ns of
        { Nothing -> (fmap g (f o)) >>= \y -> (runBuilding r) y os ns
        ; Just _ -> Left $ "Parameter \"" ++ n ++ "\" has positional argument as well named argument"
        }
    }


buildin_abs :: [Value] -> [([Char], Value)] -> Either [Char] Value
buildin_abs = overloadBuildin _int _float
    where _int = callBuildin Prelude.abs
               $ param "x" Nothing expectInteger
               $ ret IntegerVal
          _float = callBuildin Prelude.abs
                 $ param "x" Nothing expectDecimal
                 $ ret DecimalVal

buildin_attr = callBuildin lookup
             $ param "obj" Nothing expectObject
             $ param "str" Nothing expectString
             $ ret' (maybe (Left emsg) Right)
    where emsg = "Cannot find attribute in object"

_batch :: [a] -> Int -> Maybe a -> [[a]]
_batch xs n d = f xs
    where f [] = []
          f xs = (take n $ take n xs ++ (maybe [] repeat $ d)):(f $ drop n xs)

buildin_batch = callBuildin _batch
              $ param "value" Nothing ()
              $ param "linecount" Nothing expectInteger
              $ param "fill_with" (Just Nothing) (Just)
              $ ret ListVal

buildin_capitalize = callBuildin f
                   $ param "s" Nothing expectString
                   $ ret StringVal
    where f [] = []
          f (x:xs) = (toUpper x):xs

buildin_center = callBuildin f
               $ param "value" Nothing expectString
               $ param "width" (Just 80) expectInteger
               $ ret StringVal
    where f x n = let a = flip replicate ' ' $ div (max 0 (n - length x)) 2
                  in a ++ x ++ a

buildin_default = callBuildin f
                $ param "value" Nothing id
                $ param "default_value" (Just $ StringVal "") id
                $ param "boolean" (Just False) expectBool
                $ ret id
    where f x d True = case testBool x of { True -> x; False -> d}
          f x d False = case isNone x of { True -> d; False -> x}

buildin_dictsort = callBuildin f
                 $ param "value" Nothing expectDictionary
                 $ param "case_sensitive" (Just False) expectBool
                 $ param "by" (Just ()) (const $ pure ())
                 $ param "reverse" (Just False) expectBool
                 $ ret DictionaryVal
    where f :: [(Value, Value)] -> Bool -> () -> Bool -> [(Value, Value)]
          f xs c _ r = xs

buildin_escape = callBuildin (concatMap f)
               $ param "value" Nothing (\v -> expectString v <|> g v)
               $ ret StringVal
    where f '&' = "&amp;"
          f '<' = "&lt;"
          f '>' = "&gt;"
          f '\'' = "&#39;"
          f '\"' = "&quot;"
          f x = [x]
          g v = do { o <- expectObject v
                   ; w <- case lookup "__html__" o of
                            { Nothing -> fail $ "Cannot find a method \"__html__\""
                            ; Just x -> return x
                            }
                   ; h <- expectFunction w
                   ; y <- h [] []
                   ; expectString y
                   }

buildin_filesizeformat = callBuildin f
                       $ param "value" Nothing (\x -> expectInteger x <|> expectString x >>= g)
                       $ param "binary" (Just False) expectBool
                       $ ret StringVal
    where f n False = maybe (show n ++ "B") id
                    $ listToMaybe
                    $ reverse
                    $ catMaybes
                    $ fmap (\(s,p) -> case n >= (10^p) of
                        { True -> (show $ div n $ 10^p) ++ p
                        ; False -> Nothing
                        })
                    $ zip ["KB", "MB", "GB", "TB", "PB", "EB", "ZB"]
                    $ iterate 3 (3+)
          f n True = maybe (show n ++ "B") id
                   $ listToMaybe
                   $ reverse
                   $ catMaybes
                   $ fmap (\(s,p) -> case n >= (2^p) of
                        { True -> (show $ div n $ 2^p) ++ p
                        ; False -> Nothing
                        })
                   $ zip ["KiB", "MiB", "GiB", "TiB", "PiB", "EiB", "ZiB"]
                   $ iterate 10 (10+)
          g :: [Char] -> Either [Char] Integer
          g = maybe (Left $ "Cannot read Integer") (Right . fst)
            . listToMaybe
            . reads

buildin_first = callBuildin listToMaybe
              $ param "seq" Nothing expectList
              $ ret' (either (Left "Empty List") Right)

buildin_float = callBuildin f
              $ param "value" Nothing g
              $ param "default" (Just (0%0)) expectDecimal
              $ ret DecimalVal
    where g :: Value -> Either [Char] (Maybe Rational)
          g v = asDecimal v <|> ((%1) <$> asInteger v) <|> (asString v >>= readMaybe)
          f v d = maybe d Right v

buildin_forceescape = callBuildin (either (concatMap f) id)
                    $ param "value" Nothing (\x -> (Left <$> expectString x) <|>
                                                   (Right <$> g x))
                    $ ret StringVal
    where g v = do { obj <- expectObject v
                   ; mbr <- case lookup "__html__" obj of
                    { Nothing -> fail "Cannot find method \"__html__\""
                    ; Just x -> return x
                    }
                   ; fun <- expectFunction mbr
                   ; res <- fun [] []
                   ; expectString res
                   }
          f '&' = "&amp;"
          f '<' = "&lt;"
          f '>' = "&gt;"
          f '\'' = "&#39;"
          f '\"' = "&quot;"
          f x = [x]

-- TODO implementation of buildin_format

_indent :: [Char] -> Int -> Bool -> Bool -> [Char]
_indent s w True True = unlines $ fmap (f . g) $ lines s
    where g = dropWhile isSpaces
          f = ((repeat w ' ')++)
_indent s w True False = unlines $ fmap () $ lines s
    where f 
_indent s w False b = f $ lines s
    where f (l:ls) = unlines $ (l:) $ fmap (g b) ls
          f [] = []
          g True x = ((repeat w ' ')++) $ dropWhile isSpaces x
          g False x = case all isSpace x of
                    { True -> x
                    ; False -> ((repeat w ' ')++) $ dropWhile isSpace x
                    }

builtin_indent xs = foo ["s", "width", "first", "blank"] $ do
    { text <- pop >>= expectString
    ; width <- pop >>= expectInteger <|> (expectString >>= toInteger) <?> 80
    ; first <- pop >>= expectBoolean <?> False
    ; blank <- pop >>= expectBoolean <?> False
    ; return _indent text width first blank
    }

builtin_indent = _indent
           `foo` (bar "s" $ expectString)
           `foo` (bar "width" $ expectInteger <|> (expectString >>= toInteger) <?> 80)
           `foo` (bar "first" $ expectBoolean <?> False)
           `foo` (bar "blank" $ expectBoolean < False)




