module Function ( abs
                , attr
                , batch
                , capitalize
                ) where

import Value ( Value, Type, toType )
import Control.Monad.IO
import qualified Prelude ( abs )


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

builtin_capitalize :: [Value] -> Either [[Char]] Value
builtin_capitalize [StringVal x] = return $ StringVal $ f x
    where f [] = []
          f (x:xs) = (toUpper x):xs
builtin_capitalize xs = fail $ do_error "capitalize" [[StringType]] xs

builtin_center :: [Value] -> Either [[Char]] Value
builtin_center [StringVal x] = builtin_center [StringVal x, IntegerVal 80]
builtin_center [StringVal x, IntegerVal y] = return $ StringVal $ a ++ x ++ a
    where a = flip replicate ' ' $ div (max 0 (y - length x)) 2
builtin_center xs = fail $ do_error "center" [[StringType], [StringType, IntegerVal]] xs

builtin_default :: [Value] -> Either [[Char]] Value
builtin_default [NoneVal, y] = return y
builtin_default [x, y] = return x
builtin_default [NoneVal, y, BoolVal _] = return y
builtin_default [x, y, BoolVal z] = return $ case (testValue x) || not z of
    { True -> x
    ; False -> y
    }
builtin_default xs = fail $ do_error "default" [[], []] xs

builtin_dictsort :: [Value] -> Either [[Char]] Value
builtin_dictsort =

builtin_escape :: [Value] -> Either [[Char]] Value
builtin_escape [StringVal x] = return $ StringVal $ concatMap f x
    where f '&' = "&amp;"
          f '<' = "&lt;"
          f '>' = "&gt;"
          f '\'' = "&#39;"
          f '\"' = "&quot;"
          f x = [x]
builtin_escape [ObjectVal xs] = maybe (fail e) return $ lookup "__html__" xs
    where e = ["missing attribute \"__html__\""]
builtin_escape xs = fail $ do_error "ecape" [[StringType], [ObjectType]] xs

builtin_filesizeformat :: 

builtin_first = 



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




