{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import System.Console.GetOpt
import System.IO (FilePath, hPutStrLn, stderr)
import Control.Monad.Fail (MonadFail)
import Control.Monad
import System.Environment
import Data.List (intercalate)
import Text.Parsec (runParser, Stream, ParsecT, parse)
import Executable (Executable, execute)

import AST ( Expression(..)
           , Statement(..)
           , IfStatement(..)
           , ForStatement(..)
           , MacroStatement(..)
           , CallStatement(..)
           , BlockStatement(..)
           )
import Parser ( expression, baseTemplate )
import Value (Value(..))
import Exception
import Evaluatable ( Evaluatable, evaluate )
import Resolver ( runResolverT )
import Function (  )
import Location ( Location(..) )
import Error ( ExceptionT(..) )

instance Show (Expression a) where
    show (NoneExpr _) = "None"
    show (BoolExpr True _) = "True"
    show (BoolExpr False _) = "False"
    show (IntegerExpr x _) = show x
    show (NumberExpr x _) = show x
    show (StringExpr x _) = show x
    show (SymbolExpr x _) = x
    show (NegateExpr x _) = "negate(" ++ show x ++ ")"
    show (ComplementExpr x _) = "not(" ++ show x ++ ")"
    show (MemberExpr m x _) = "member(" ++ show x ++ ", " ++ m ++ ")"
    show (IndexExpr i x _) = "at(" ++ show x ++ ", " ++ show i ++ ")"
    show (MultiplyExpr l r _) = "multiply(" ++show l ++ ", " ++ show r ++ ")"
    show (DivideExpr l r _) = "divide(" ++ show l ++ ", " ++ show r ++ ")"
    show (IntegralDivideExpr l r _) = "integral_divide(" ++ show l ++ ", " ++ show r ++ ")"
    show (ModuloExpr l r _) = "modulo(" ++ show l ++ ", " ++ show r ++ ")"
    show (AddExpr l r _) = "add(" ++ show l ++ ", " ++ show r ++ ")"
    show (SubtractExpr l r _) = "subtract(" ++ show l ++ ", " ++ show r ++ ")"
    show (SameExpr l r _) = "same(" ++ show l ++ ", " ++ show r ++ ")"
    show (NotSameExpr l r _) = "not_same(" ++ show l ++ ", " ++ show r ++ ")"
    show (InExpr l r _) = "in(" ++ show l ++ ", " ++ show r ++ ")"
    show (NotInExpr l r _) = "not_in(" ++ show l ++ ", " ++ show r ++ ")"
    show (IsExpr l r _) = "is(" ++ show l ++ ", " ++ show r ++ ")"
    show (IsNotExpr l r _) = "is_not(" ++ show l ++ ", " ++ show r ++ ")"
    show (LessExpr l r _) = "less(" ++ show l ++ ", " ++ show r ++ ")"
    show (LessEqualExpr l r _) = "less_eq(" ++ show l ++ ", " ++ show r ++ ")"
    show (GreaterExpr l r _) = "greater(" ++ show l ++ ", " ++ show r ++ ")"
    show (GreaterEqualExpr l r _) = "greater_eq(" ++ show l ++ ", " ++ show r ++ ")"
    show (AndExpr l r _) = "and(" ++ show l ++ ", " ++ show r ++ ")"
    show (OrExpr l r _) = "or(" ++ show l ++ ", " ++ show r ++ ")"
    show (ComposeExpr l r _) = "compose(" ++ show l ++ ", " ++ show r ++ ")"
    show (SliceExpr b e s x _) = "slice(" ++ show x ++ ", " ++ (maybe "Nothing" show b) ++ ", " ++ (maybe "Nothing" show e) ++ ", " ++ (maybe "Nothing" show s) ++ ")"
    show (ListExpr xs _) = "list(" ++ (intercalate ", " $ fmap show xs) ++ ")"
    show (DictionaryExpr xs _) = "dictionary(" ++ (intercalate ", " $ flip fmap xs $ \(k,v) -> show k ++ ": " ++ show v) ++ ")"
    show (ObjectExpr xs _) = "object(" ++ (intercalate ", " $ flip fmap xs $ \(k,v) -> show k ++ ": " ++ show v) ++ ")"
    show (CallExpr ps x _) = "call(" ++ show x ++ (concat $ flip fmap ps $ \p -> ", " ++ show p) ++ ")"


toShow :: Statement a -> [[Char]]
toShow (LiteralStmt x _) = ["Literal: " ++ show x]
toShow (InterpolationStmt x _) = ["Interpolation: " ++ show x]
toShow (CommentStmt x _) = ["Comment: " ++ show x]
toShow (LineBreakStmt x _) = ["LineBreak :" ++ show x]
toShow (IndentationStmt x _) = ["Indentation: " ++ show x]
toShow (ForStmt x _) = ["For: " ++ s ++ " <- " ++ e ++ f ++ r] ++ b
    where s = intercalate ", " $ forSymbols x
          e = show $ forRange x
          f = maybe "; select all" (\y-> "; select " ++ show y) $ forFilter x
          r = case forRecursion x of {True -> "; recurse"; False -> "; dont recurse"; }
          b = fmap ("\t"++) $ concatMap toShow $ forBody x
toShow (IfStmt x _) =  (a $ ifBranch x)
                    ++ (concatMap b $ elifBranches x)
                    ++ (c $ elseBranch x)
    where a (e, sts, _) = ["if: " ++ show e] ++ (fmap ("\t"++) $ concatMap toShow sts)
          b (e, sts, _) = ["elif: " ++ show e] ++ (fmap ("\t"++) $ concatMap toShow sts)
          c Nothing     = []
          c (Just (sts,_)) = ["else"] ++ (fmap ("\t"++) $ concatMap toShow sts)
toShow (MacroStmt x _) = ["Macro: " ++ n ++ "(" ++ ps ++ ")"] ++ bs
    where n = macroName x
          ps = intercalate ", " $ macroArguments x
          bs = fmap ("\t"++) $ concatMap toShow $ macroBody x
toShow (CallStmt x _) =  ["Call: (" ++ as ++ ") -> " ++ n ++ "(" ++  ps ++ ")"]
                      ++ b
    where as = maybe "" (intercalate ", ") $ callArguments x
          n = callName x
          ps = intercalate ", " $ fmap show $ callParameters x
          b = fmap ("\t"++) $ concatMap toShow $ callInput x
toShow (FilterStmt n sts _) = ["Filter: " ++ n] ++ (fmap ("\t"++) $ concatMap toShow sts)
toShow (ExprSetStmt ns e zs _) =
    ["ExprSet:" ++ (intercalate ", " ns) ++ " <- " ++ show e] ++
    (fmap ("\t"++) $ concatMap toShow zs)
toShow (BlockSetStmt n xs zs _) =
    ["BlockSet:" ++ n ++ " <- " ++ "some statements" ] ++
    (fmap ("\t"++) $ concatMap toShow zs)
toShow (IncludeStmt x a b _) = ["Include: " ++ show x ++ (a' a) ++ (b' b)]
    where a' True = " ignore missing"
          a' False = " don't ignore missing"
          b' Nothing = " default contexting"
          b' (Just True) = " with context"
          b' (Just False) = " without context"
toShow (ImportStmt x n _) = ["Import: " ++ show x ++ " as " ++ n]
toShow (QualifiedImportStmt x ts _) = ["Import: " ++ show x ++ " as " ++ n]
    where n = intercalate ", " $ fmap (\(a,b) -> a ++ (maybe "" (" as "++) b)) ts
toShow (RawStmt n _) = ["Raw: " ++ show n]
toShow (BlockStmt x _) =  ["Block: " ++ (blockName x) ++ (s $ blockScoped x) ++ (r $ blockRequired x)]
                       ++ b
    where s True = " scoped"
          s False = " unscoped"
          r True = " required"
          r False = " unrequired"
          b = fmap ("\t"++) $ concatMap toShow $ blockBody x

instance Show (Statement a) where
    show = unlines . toShow


data Options = Options
    { optConfig :: Maybe Value
    , optAllowedPaths :: [[Char]]
    }

defaultOptions = Options
    { optConfig = Nothing
    , optAllowedPaths = []
    }

options :: [OptDescr (Options -> IO Options)]
options =
    [ Option "j" ["json-config"]
             (ReqArg (\f o -> return o) "FILE")
             "read global variables as JSON document from FILE"
    , Option "a" ["allow-path"]
             (ReqArg (\p o -> return $ o {optAllowedPaths = p : optAllowedPaths o}) "PATH")
             "allow to read PATH"
    ]



initialSymbols :: (Monad m) => m [([Char], Value)]
-- initialSymbols = return [("abs", FunctionVal (BuiltinLocation "buildin_abs") buildin_abs)]
initialSymbols = return []


main :: IO ()
main = do
    args <- getArgs
    input <- getContents
    case parse baseTemplate "input" input of
        { Left err -> putStrLn $ "Error: " ++ show err
        ; Right xs -> do
            { symbols <- initialSymbols
            ; exception <- runException $ runResolverT (execute xs) [symbols]
            ; case exception of
                { Left e -> hPutStrLn stderr $ "Got some error"
                ; Right output -> putStrLn $ concat output
                }
            }
        }
