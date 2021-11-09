{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Evaluatable ( Evaluatable, evaluate) where

import AST ( Expression(..), Statement(..) )
import Value ( Value(..)
             , expectNone
             , expectBool
             , expectInteger
             , expectDecimal
             , expectString
             , expectList
             , expectDictionary
             , expectObject
             , expectFunction
             , noneVal
             , boolVal
             , integerVal
             , decimalVal
             , stringVal
             , listVal
             , dictionaryVal
             , objectVal
             , functionVal
             , FromValue
             )
import qualified Control.Monad.Fail as F
-- import Resolver ( MonadResolver, resolveName )
import Control.Applicative ( Alternative, empty, (<|>) )
import Data.Ratio ( numerator )
import Data.Maybe ( listToMaybe )

anyOf :: Alternative f => [f a] -> f a
anyOf [] = empty
anyOf (x:xs) = x <|> anyOf xs


toBool :: (FromValue Maybe a) => a -> Bool
toBool x = maybe False id $ anyOf
    [ False           <$  expectNone x
    , id              <$> expectBool x
    , (0/=)           <$> expectInteger x
    , (0/=).numerator <$> expectDecimal x
    , (not.null)      <$> expectString x
    , (not.null)      <$> expectList x
    , (not.null)      <$> expectObject x
    ]

class Evaluatable e m where
    evaluate :: e a -> m (Value, a)


instance (Monad m, Alternative m, F.MonadFail m) => Evaluatable Expression m where
    evaluate (NoneExpr a) = return (noneVal, a)
    evaluate (BoolExpr b a) = return (boolVal b, a)
    evaluate (IntegerExpr i a) = return (integerVal i, a)
    evaluate (NumberExpr f a) = return (decimalVal f, a)
    evaluate (StringExpr s a) = return (stringVal s, a)
    evaluate (SymbolExpr s a) = fmap (\v -> (v,a)) $ resolveName s
        where resolveName _ = return $ integerVal 123
    evaluate (ListExpr es a) = do
        { es' <- mapM evaluate es
        ; return (listVal $ fmap fst es', a)
        }
    evaluate (DictionaryExpr es a) = do
        { es' <- mapM (\(a,b) -> (\c d -> (fst c, fst d)) <$> evaluate a <*> evaluate b) es
        ; return (dictionaryVal es', a)
        }
    evaluate (NegateExpr e a) = do
        { (e', ea) <- evaluate e
        ; n <- anyOf
            [ (integerVal . negate) <$> expectInteger e'
            , (decimalVal . negate) <$> expectDecimal e'
            ]
        ; return (n, a)
        }
    evaluate (ComplementExpr e a) = do
        { (e', ea) <- evaluate e
        ; c <- anyOf
            [ True            <$  expectNone e'
            , not             <$> expectBool e'
            , (0==)           <$> expectInteger e'
            , (0==).numerator <$> expectDecimal e'
            , null            <$> expectString e'
            , null            <$> expectList e'
            , null            <$> expectObject e'
            , False           <$  expectFunction e'
            ]
        ; return (boolVal c, a)
        }
    evaluate (MemberExpr n e a) = do
        { (e', ea) <- evaluate e
        ; os <- expectObject e'
        ; x <- case lookup n os of
            { Just x -> return x
            ; Nothing -> F.fail $ "Cannot find member " ++ show n
            }
        ; return (x, a)
        }
    evaluate (IndexExpr i e a) = do
        { (e', ea) <- evaluate e
        ; (i', ia) <- evaluate i
        ; x <- anyOf
            [ do{ ls <- expectList e'
                ; n <- expectInteger i'
                ; case listToMaybe $ drop (fromEnum n) ls of
                    { Just x -> return x
                    ; Nothing -> F.fail $ "Cannot get index " ++ show n
                    }
                }
            , do{ os <- expectDictionary e'
                ; case lookup i' os of
                    { Just x -> return x
                      -- TODO fix formating of i'
                    ; Nothing -> F.fail $ "Cannot find " ++ "<value of i>" ++ " as key"
                    }
                }
            ]
        ; return (x, a)
        }
    evaluate (AddExpr l r a) = do
        { (l', la) <- evaluate l
        ; (r', ra) <- evaluate r
        ; x <- anyOf
            [ (integerVal.).(+)
                <$> expectInteger l'
                <*> expectInteger r'
            , (decimalVal.).(+)
                <$> expectDecimal l'
                <*> expectDecimal r'
            , (decimalVal.).(+)
                <$> (toRational <$> expectInteger l')
                <*> expectDecimal r'
            , (decimalVal.).(+)
                <$> expectDecimal l'
                <*> (toRational <$> expectInteger r')
            ]
        ; return (x, a)
        }
    evaluate (SubtractExpr l r a) = do
        { (l', la) <- evaluate l
        ; (r', ra) <- evaluate r
        ; x <- anyOf
            [ (integerVal.).(-)
                <$> expectInteger l'
                <*> expectInteger r'
            , (decimalVal.).(-)
                <$> expectDecimal l'
                <*> expectDecimal r'
            , (decimalVal.).(-)
                <$> (toRational <$> expectInteger l')
                <*> expectDecimal r'
            , (decimalVal.).(-)
                <$> expectDecimal l'
                <*> (toRational <$> expectInteger r')
            ]
            ; return (x, a)
        }
    evaluate (MultiplyExpr l r a) = do
        { (l', la) <- evaluate l
        ; (r', ra) <- evaluate r
        ; x <- anyOf
            [ (integerVal.).(*)
                <$> expectInteger l'
                <*> expectInteger r'
            , (decimalVal.).(*)
                <$> expectDecimal l'
                <*> expectDecimal r'
            , (decimalVal.).(*)
                <$> (toRational <$> expectInteger l')
                <*> expectDecimal r'
            , (decimalVal.).(*)
                <$> expectDecimal l'
                <*> (toRational <$> expectInteger r')
            ]
        ; return (x, a)
        }
    evaluate (DivideExpr l r a) = do
        { (l', la) <- evaluate l
        ; (r', ra) <- evaluate r
        ;       (\lh rh -> (decimalVal $ lh / rh, a))
            <$> anyOf [ toRational <$> expectInteger l', expectDecimal l']
            <*> anyOf [ toRational <$> expectInteger r', expectDecimal r']
        }
    evaluate (IntegralDivideExpr l r a) = do
        { (l', la) <- evaluate l
        ; (r', ra) <- evaluate r
        ; x <- anyOf
            {- TODO test on division by zero -}
            [ (integerVal.).div
                <$> expectInteger l'
                <*> expectInteger r'
            , ((integerVal . floor) .).(/)
                <$> (toRational <$> expectInteger l')
                <*> expectDecimal r'
            , ((integerVal . floor) .).(/)
                <$> expectDecimal l'
                <*> (toRational <$> expectInteger r')
            , ((integerVal . floor) .).(/)
                <$> expectDecimal l'
                <*> expectDecimal r'
            ]
        ; return (x, a)
        }
    evaluate (ModuloExpr l r a) = do
        { (l', la) <- evaluate l
        ; (r', ra) <- evaluate r
        ; x <- anyOf
            {- TODO test modulo by zero -}
            [ (integerVal.).(mod)
                <$> expectInteger l'
                <*> expectInteger r'
            , (decimalVal.).(\a b -> a - (fromIntegral $ floor $ a / b) * b)
                <$> expectDecimal l'
                <*> (toRational <$> expectInteger r')
            , (decimalVal.).(\a b -> a - (fromIntegral $ floor $ a / b) * b)
                <$> (toRational <$> expectInteger l')
                <*> expectDecimal r'
            , (decimalVal.).(\a b -> a - (fromIntegral $ floor $ a / b) * b)
                <$> expectDecimal l'
                <*> expectDecimal r'
            ]
        ; return (x, a)
        }
    evaluate (SameExpr l r a) = do
        { (l', la) <- evaluate l
        ; (r', ra) <- evaluate r
        ; return (boolVal $ l' == r', a)
        }
    evaluate (NotSameExpr l r a) = do
        { (l', la) <- evaluate l
        ; (r', ra) <- evaluate r
        ; return (boolVal $ l' /= r', a)
        }
    evaluate (LessExpr l r a) = do
        { (l', la) <- evaluate l
        ; (r', ra) <- evaluate r
        ; x <- anyOf
            [ (<) <$> expectInteger l' <*> expectInteger r'
            , (<) <$> expectDecimal l' <*> expectDecimal r'
            , (<) <$> (toRational <$> expectInteger l') <*> expectDecimal r'
            , (<) <$> expectDecimal l' <*> (toRational <$> expectInteger r')
            , (<) <$> expectString l' <*> expectString r'
            , ((,) <$> expectList l' <*> expectList r') >> F.fail "Missing implementation on List comparison for <"
            , ((,) <$> expectDictionary l' <*> expectDictionary r') >> F.fail "Missing implementation on List comparison for <"
            ]
        ; return (boolVal x, a)
        }
    evaluate (LessEqualExpr l r a) = do
        { (l', la) <- evaluate l
        ; (r', ra) <- evaluate r
        ; x <- anyOf
            [ (<=) <$> expectInteger l' <*> expectInteger r'
            , (<=) <$> expectDecimal l' <*> expectDecimal r'
            , (<=) <$> (toRational <$> expectInteger l') <*> expectDecimal r'
            , (<=) <$> expectDecimal l' <*> (toRational <$> expectInteger r')
            , (<=) <$> expectString l' <*> expectString r'
            , ((,) <$> expectList l' <*> expectList r') >> F.fail "Missing implementation on List comparison for <="
            , ((,) <$> expectDictionary l' <*> expectDictionary r') >> F.fail "Missing implementation on List comparison for <="
            ]
        ; return (boolVal x, a)
        }
    evaluate (GreaterExpr l r a) = do
        { (l', la) <- evaluate l
        ; (r', ra) <- evaluate r
        ; x <- anyOf
            [ (>) <$> expectInteger l' <*> expectInteger r'
            , (>) <$> expectDecimal l' <*> expectDecimal r'
            , (>) <$> (toRational <$> expectInteger l') <*> expectDecimal r'
            , (>) <$> expectDecimal l' <*> (toRational <$> expectInteger r')
            , (>) <$> expectString l' <*> expectString r'
            , ((,) <$> expectList l' <*> expectList r') >> F.fail "Missing implementation on List comparison for >"
            , ((,) <$> expectDictionary l' <*> expectDictionary r') >> F.fail "Missing implementation on List comparison for >"
            ]
        ; return (boolVal x, a)
        }
    evaluate (GreaterEqualExpr l r a) = do
        { (l', la) <- evaluate l
        ; (r', ra) <- evaluate r
        ; x <- anyOf
            [ (>=) <$> expectInteger l' <*> expectInteger r'
            , (>=) <$> expectDecimal l' <*> expectDecimal r'
            , (>=) <$> (toRational <$> expectInteger l') <*> expectDecimal r'
            , (>=) <$> expectDecimal l' <*> (toRational <$> expectInteger r')
            , (>=) <$> expectString l' <*> expectString r'
            , ((,) <$> expectList l' <*> expectList r') >> F.fail "Missing implementation on List comparison for >="
            , ((,) <$> expectDictionary l' <*> expectDictionary r') >> F.fail "Missing implementation on List comparison for >="
            ]
        ; return (boolVal x, a)
        }
    evaluate (InExpr l r a) = do
        { (l', la) <- evaluate l
        ; (r', ra) <- evaluate r
        ; x <- anyOf
            [ (l' `elem`) <$> expectList r'
            , (l' `elem`).(fmap fst) <$> expectDictionary r'
            ]
        ; return (boolVal x, a)
        }
    evaluate (NotInExpr l r a) = do
        { (l', la) <- evaluate l
        ; (r', ra) <- evaluate r
        ; x <- anyOf
            [ (l' `notElem`) <$> expectList r'
            , (l' `notElem`).(fmap fst) <$> expectDictionary r'
            ]
        ; return (boolVal x, a)
        }
    evaluate (IsExpr l r a) = F.fail $ "The \"is\" operator is not supported so far!"
    evaluate (IsNotExpr l r a) = F.fail $ "The \"is not\" operator is not supported so far!"
    evaluate (AndExpr l r a) = g <$> evaluate l <*> evaluate r
        where g l' r' = (boolVal $ (toBool $ fst l') && (toBool $ fst r'), a)
    evaluate (OrExpr l r a) = g <$> evaluate l <*> evaluate r
        where g l' r' = (boolVal $ (toBool $ fst l') || (toBool $ fst r'), a)
    evaluate (SliceExpr f l i e a) = F.fail $ "Slice is not supported so far!"
    {-
        =   \e' f' l' i' -> do
            { anyOf
                [ expectList
                , 
                ]
            ; 
            }
        <$> do
            { e' <- (fst <$> evaluate e)
            ; anyOf
                [  <$> expectList e'
                , 
                ]
            }
        <*> (fst <$> traverse evaluate f)
        <*> (fst <$> traverse evaluate l)
        <*> (fst <$> traverse evaluate i)
    -}
    evaluate (TernaryExpr c p n a) = do
        { (c', ca) <- evaluate c
        ; cond <- anyOf
            [ False           <$  expectNone c'
            , id              <$> expectBool c'
            , (0/=)           <$> expectInteger c'
            , (0/=).numerator <$> expectDecimal c'
            , not.null        <$> expectString c'
            , not.null        <$> expectList c'
            , not.null        <$> expectObject c'
            ]
        ; fmap (\x -> (fst x,a)) $ case cond of
            { True -> evaluate p
            ; False -> evaluate n
            }
        }
    evaluate (CallExpr ps c a) = do
        { (c', ca) <- evaluate c
        ; f <- expectFunction c'
        ; F.fail "have not implemented function call!"
        }
    evaluate (LambdaExpr ns b a) = F.fail "Lambda expression is not supported so far"
    evaluate (ComposeExpr x f a) = F.fail "Compose expression is not supported so far"
    
