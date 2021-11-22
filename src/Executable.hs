{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Executable ( Executable , execute ) where

import Control.Monad
import Control.Applicative ( Alternative )
import Control.Monad.Extra (concatMapM )
import qualified Control.Monad.Fail as F
import Resolver ( MonadResolver, resolveName, withNames )
import AST ( Expression(..), Statement(..) )
import Evaluatable ( Evaluatable, evaluate )
import Value ( Value
             , printPretty
             , printCompact
             , expectList
             , noneVal
             , stringVal
             )


anyOf :: [Maybe a] -> Maybe a
anyOf [] = Nothing
anyOf (Nothing:xs) = anyOf xs
anyOf ((Just x):xs) = Just x

{-

executeIf :: [(Expression a, [Statement a], a)] -> Maybe ([Statement a], a) -> m [[Char]]
executeIf [] Nothing = return []
executeIf [] (Just (xs, a)) = execute xs
executeIf ((expr, stmts, a):xs) y = do
    val <- evaluate expr
    let isOkay = anyOf
        [ False          <$  asNone val
        , id             <$> asBool val
        , (0/=)          <$> asInteger val
        , (0/=)          <$> asDecimal val
        , ((0/=).length) <$> asString val
        , ((0/=).length) <$> asList val
        , ((0/=).length) <$> asDictionary val
        , ((0/=).length) <$> asObject val
        , True           <$  asFunction val
        ]
    case isOkay of
        { Nothing -> error "Debug: should not appear!"
        ; Just False -> executeIf xs y
        ; Just True -> execute stmts
        }


executeFor :: ForStatement a -> m [[Char]]
executeFor x = do
    values <- evaluate $ forRange x
    range <- anyOf
        [ expectList values >>= \xs f -> forM xs $ \x ->
        , expectDictionary values >>=
        , expectObject values >>=
    expectList :: a -> n [a]
    expectDictionary :: a -> n [(a, a)]
    expectObject :: a -> n [([Char], a)]
        ,
        ]

defineMacro :: (Monad m, MonadFail m, MonadResolver m)
    => MacroStatement a
    -> m ([Char], [Value] -> m [[Char]])
defineMacro x = return $ (macroName x,) $ \args -> do
    unless (length args == length $ macroArguments x) $
        fail "Insufficient many parameters; "
    withNames (flip zip args $ macroArguments x) $ macroBody x

executeCall :: () => CallStatement a -> m [[Char]]
executeCall x = do
    resolved <- resolveName $ callName x
    macro <- expectFunction resolved
    params <- mapM evaluate $ callParameters x
    let callFunction args = do
        unless (length args >= maybe 0 length $ callArguments x) $
            fail "Insufficient many parameters"
        let names = (zip (maybe [] id $ callArguments x) args)
        withNames names $ callInput x
    let names = 
    withNames names $ macro params
-}




mapStmt :: ( Monad m
           , F.MonadFail m
           , Alternative m
           , MonadResolver Value m
           ) => Statement a -> m [[Char]]
mapStmt (LiteralStmt x _) = return [x]
mapStmt (InterpolationStmt x _) = fmap f $ evaluate x
    where f = (:[]) . printCompact . fst
mapStmt (CommentStmt x _) = return []
mapStmt (LineBreakStmt x _) = return [x]
mapStmt (IndentationStmt x _) = return [x]
mapStmt (ForStmt x a) = fail "ForStmt are not supported yet"
mapStmt (IfStmt x a) = fail "IfStmt are not supported yet"
mapStmt (MacroStmt x a) = fail "MacroStmt are not supported yet"
mapStmt (CallStmt x a) = fail "CallStmt are not supported yet"
mapStmt (FilterStmt n xs a) = fail "FilterStmt are not supported yet"
mapStmt (ExprSetStmt ns e zs a) = do
    (e',_) <- evaluate e
    ms <- case ns of
        { [] -> fail "Internal Error: should not happen!!!"
        ; n:[] -> return [(n, e')]
        ; ns -> do
            y <- expectList e'
            return $ zip ns $ (y ++ (repeat noneVal))
        }
    withNames ms $ execute zs
mapStmt (BlockSetStmt n xs zs a) = do
    ys <- execute xs
    let y = stringVal $ concat ys
    withNames [(n, y)] $ execute zs
mapStmt (IncludeStmt x f1 f2 a) = fail "IncludeStmt are not supported yet"
mapStmt (ImportStmt x n a) = fail "Import Stmt are not supported yet"
mapStmt (QualifiedImportStmt x ns a) = fail "Qualified Import Stmt are not supported yet"
mapStmt (RawStmt x _) = return [x]
mapStmt (BlockStmt x a) = fail "BlockStmt are not supported so far"


class Executable e m where
    execute :: [e a] -> m [[Char]]

instance ( Monad m
         , F.MonadFail m
         , Alternative m
         , MonadResolver Value m
         ) => Executable Statement m where
    execute = concatMapM mapStmt
