module Function ( abs
                , attr
                , batch
                , capitalize
                ) where

import Value ( Value, Type, toType )
import Control.Monad.IO
import qualified Prelude ( abs )


do_error :: [Char] -> [Value] -> [[Type]] -> [Char]
do_error n vs tts = unlines $  [f, "Expected one of :"] ++ fmap h tts
    where f = "Got" ++ n ++ "(" ++ (intercalate ", " $ fmap (show . toType) vs) ++ ")"
          h ts = "\t" ++ n ++ "(" ++ (intercalate ", " $ fmap show ts) ++ ")"

builtin_abs :: [Value] -> Either [[Char]] Value
builtin_abs [IntegerVal x] = return $ IntegerVal $ Prelude.abs x
builtin_abs [DecimalVal x] = return $ DecimalVal $ Prelude.abs x
builtin_abs xs = fail $ do_error "abs" [[IntegerType], [DecimalType]] xs

builtin_attr :: [Value] -> Either [[Char]] Value
builtin_attr [ObjectVal xs, StringVal y] = maybe (fail esmg) return $ lookup y xs
    where esmg = "No attribute named " ++ show y ++ " in object"
builtin_attr xs = fail $ do_error "attr" [[ObjectType, StringType]] xs

builtin_batch :: [Value] -> Either [[Char]] Value
builtin_batch

builtin_capitalize :: [Value] -> Either [[Char]] Value
builtin_capitalize [StringVal x] = return $ StringVal $ f x
    where f [] = []
          f (x:xs) = (toUpper x):xs
builtin_capitalize xs = fail $ do_error "capitalize" [[StringType]] xs

builtin_center :: [Value] -> Either [[Char]] Value
builtin_center [StringVal x] = builtin_center [StringVal x, IntegerVal 80]
builtin_center [StringVal x, IntegerVal y] = return ""
builtin_center xs = fail $ do_error "center" [[StringType], [StringType, IntegerVal]] xs



