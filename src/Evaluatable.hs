{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Evaluatable ( Evaluatable, evaluate) where

import AST ( Expression(..), Statement(..) )
import Value ( Value(..)
             -- , Function(..)
             , expectNone
             , expectBool
             , expectInteger
             , expectFloat
             , expectString
             , expectList
             , expectDictionary
             , expectObject
             -- , expectFunction
             , expectBuildin
             , testValue
             , toBool
             , FromValue
-- , NoneVal
-- , BoolVal
-- , IntegerVal
-- , FloatVal
-- , StringVal
-- , ListVal
-- , DictionaryVal
-- , ObjectVal
-- , FunctionVal
             )
import Failure ( MonadFailure, doFail )
import Control.Applicative ( Alternative, empty, (<|>) )
import Data.Ratio ( numerator )
import Data.Maybe ( listToMaybe )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Evaluation ( Evaluation(..) )
import Error ( Error, collectError, throwError, traceError, Exception(..), ExceptionT(..) )
import Data.List.NonEmpty ( (<|), NonEmpty( (:|) ) )
import Control.Monad.Identity ( Identity(..) )
import Function ( callFunction )
import Buildin ( runBuildin )
import Resolver ( MonadResolver, resolveName, ResolverT(..), liftResolverT )



class Evaluatable e m where
    evaluate :: e a -> m (Value, a)


instance ( Monad m
         , MonadFailure m
         , FromValue m Value
         , MonadResolver Value m
         , Error t m
         ) => Evaluatable Expression m where
    evaluate (NoneExpr a) = return (NoneVal, a)
    evaluate (BoolExpr b a) = return (BoolVal b, a)
    evaluate (IntegerExpr i a) = return (IntegerVal i, a)
    evaluate (NumberExpr f a) = return (FloatVal f, a)
    evaluate (StringExpr s a) = return (StringVal s, a)
    evaluate (SymbolExpr s a) = fmap (\v -> (v,a)) $ resolveName s
    evaluate (ListExpr es a) =
        do
        { es' <- flip mapM (zip es $ iterate (+1) 1)
                    $ \(e, i) -> evaluate e
        ; return (ListVal $ fmap fst es', a)
        }
    evaluate (DictionaryExpr es a) =
        do
        { es' <- flip mapM (zip es $ iterate (+1) 1)
                    $ \((b,c), i) -> (\d e -> (fst d, fst e)) <$>  evaluate b <*> evaluate c
        ; return (DictionaryVal es', a)
        }
    evaluate (NegateExpr e a) =
        do
        { (e', ea) <- evaluate e
        ; n <- collectError
                $  ((IntegerVal . negate) <$> expectInteger e')
                <| ((FloatVal . negate)   <$> expectFloat e')
                :| []
        ; return (n, a)
        }
    evaluate (ComplementExpr e a) = do
        { (e', ea) <- evaluate e
        ; c <- collectError
                $  (True    <$  expectNone e')
                <| (not     <$> expectBool e')
                <| ((0==)   <$> expectInteger e')
                <| ((0==)   <$> expectFloat e')
                <| (null    <$> expectString e')
                <| (null    <$> expectList e')
                <| (null    <$> expectObject e')
                -- <| (False   <$  expectFunction e')
                :| []
        ; return (BoolVal c, a)
        }
    evaluate (MemberExpr n e a) = do
        { (e', ea) <- evaluate e
        ; os <- expectObject e'
        ; x <- case lookup n os of
            { Just x -> return x
            ; Nothing -> throwError
                         $ "Cannot find member " ++ show n
            }
        ; return (x, a)
        }
    evaluate (IndexExpr i e a) = do
        { (e', ea) <- evaluate e
        ; (i', ia) <- evaluate i
        ; x <- collectError
                $  do{ ls <- expectList e'
                     ; n <- expectInteger i'
                     ; case listToMaybe $ drop (fromEnum n) ls of
                        { Just x -> return x
                        ; Nothing -> doFail $ "Cannot get index " ++ show n
                        }
                     }
                <| do{ os <- expectDictionary e'
                     ; case lookup i' os of
                        { Just x -> return x
                          --TODO fix formating of i'
                        ; Nothing -> doFail $ "Cannot find " ++ "value of i>" ++ " as key"
                        }
                     }
                :| []
        ; return (x, a)
        }
    evaluate (AddExpr l r a) = do
        { (l', la) <- evaluate l
        ; (r', ra) <- evaluate r
        ; x <- collectError
                $  ( (IntegerVal.).(+)
                        <$> expectInteger l'
                        <*> expectInteger r')
                <| ( (FloatVal.).(+)
                        <$> expectFloat l'
                        <*> expectFloat r')
                <| ( (FloatVal.).(+)
                        <$> (fromIntegral <$> expectInteger l')
                        <*> expectFloat r')
                <| ( (FloatVal.).(+)
                        <$> expectFloat l'
                        <*> (fromIntegral <$> expectInteger r'))
                :| []
        ; return (x, a)
        }
    evaluate (SubtractExpr l r a) = do
        { (l', la) <- evaluate l
        ; (r', ra) <- evaluate r
        ; x <- collectError
            $  ( (IntegerVal.).(-)
                <$> expectInteger l'
                <*> expectInteger r')
            <| ( (FloatVal.).(-)
                <$> expectFloat l'
                <*> expectFloat r')
            <| ( (FloatVal.).(-)
                <$> (fromIntegral <$> expectInteger l')
                <*> expectFloat r')
            <| ( (FloatVal.).(-)
                <$> expectFloat l'
                <*> (fromIntegral <$> expectInteger r'))
            :| []
        ; return (x, a)
        }
    evaluate (MultiplyExpr l r a) = do
        { (l', la) <- evaluate l
        ; (r', ra) <- evaluate r
        ; x <- collectError
            $  ( (IntegerVal.).(*)
                <$> expectInteger l'
                <*> expectInteger r')
            <| ( (FloatVal.).(*)
                <$> expectFloat l'
                <*> expectFloat r')
            <| ( (FloatVal.).(*)
                <$> (fromIntegral <$> expectInteger l')
                <*> expectFloat r')
            <| ( (FloatVal.).(*)
                <$> expectFloat l'
                <*> (fromIntegral <$> expectInteger r'))
            :| []
        ; return (x, a)
        }
    evaluate (DivideExpr l r a) = do
        { (l', la) <- evaluate l
        ; (r', ra) <- evaluate r
        ;       (\lh rh -> (FloatVal $ lh / rh, a))
            <$> collectError (  (fromIntegral <$> expectInteger l')
                             <| (expectFloat l')
                             :| []
                             )
            <*> collectError (  (fromIntegral <$> expectInteger r')
                             <| (expectFloat r')
                             :| []
                             )
        }
    evaluate (IntegralDivideExpr l r a) = do
        { (l', la) <- evaluate l
        ; (r', ra) <- evaluate r
        ; x <- collectError 
            {- TODO test on division by zero -}
            $  ( (IntegerVal.).div
                    <$> expectInteger l'
                    <*> expectInteger r')
            <| ( ((IntegerVal . floor) .).(/)
                    <$> (fromIntegral <$> expectInteger l')
                    <*> expectFloat r')
            <| ( ((IntegerVal . floor) .).(/)
                    <$> expectFloat l'
                    <*> (fromIntegral <$> expectInteger r'))
            <| ( ((IntegerVal . floor) .).(/)
                    <$> expectFloat l'
                    <*> expectFloat r')
            :| []
        ; return (x, a)
        }
    evaluate (ModuloExpr l r a) = do
        { (l', la) <- evaluate l
        ; (r', ra) <- evaluate r
        ; x <- collectError
            {- TODO test modulo by zero -}
            $  ( (IntegerVal.).(mod)
                    <$> expectInteger l'
                    <*> expectInteger r')
            <| ( (FloatVal.).(\a b -> a - (fromIntegral $ floor $ a / b) * b)
                    <$> expectFloat l'
                    <*> (fromIntegral <$> expectInteger r'))
            <| ( (FloatVal.).(\a b -> a - (fromIntegral $ floor $ a / b) * b)
                    <$> (fromIntegral <$> expectInteger l')
                    <*> expectFloat r')
            <| ( (FloatVal.).(\a b -> a - (fromIntegral $ floor $ a / b) * b)
                    <$> expectFloat l'
                    <*> expectFloat r')
            :| []
        ; return (x, a)
        }
    evaluate (SameExpr l r a) = do
        { (l', la) <- evaluate l
        ; (r', ra) <- evaluate r
        ; return (BoolVal $ l' == r', a)
        }
    evaluate (NotSameExpr l r a) = do
        { (l', la) <- evaluate l
        ; (r', ra) <- evaluate r
        ; return (BoolVal $ l' /= r', a)
        }
    evaluate (LessExpr l r a) = do
        { (l', la) <- evaluate l
        ; (r', ra) <- evaluate r
        ; x <- collectError
            $  ( (<) <$> expectInteger l' <*> expectInteger r')
            <| ( (<) <$> expectFloat l' <*> expectFloat r')
            <| ( (<) <$> (fromIntegral <$> expectInteger l') <*> expectFloat r')
            <| ( (<) <$> expectFloat l' <*> (fromIntegral <$> expectInteger r'))
            <| ( (<) <$> expectString l' <*> expectString r')
            <| ( ((,) <$> expectList l' <*> expectList r') >> doFail "Missing implementation on List comparison for <")
            <| ( ((,) <$> expectDictionary l' <*> expectDictionary r') >> doFail "Missing implementation on List comparison for <")
            :| []
        ; return (BoolVal x, a)
        }
    evaluate (LessEqualExpr l r a) = do
        { (l', la) <- evaluate l
        ; (r', ra) <- evaluate r
        ; x <- collectError
            $  ( (<=) <$> expectInteger l' <*> expectInteger r')
            <| ( (<=) <$> expectFloat l' <*> expectFloat r')
            <| ( (<=) <$> (fromIntegral <$> expectInteger l') <*> expectFloat r')
            <| ( (<=) <$> expectFloat l' <*> (fromIntegral <$> expectInteger r'))
            <| ( (<=) <$> expectString l' <*> expectString r')
            <| ( ((,) <$> expectList l' <*> expectList r') >> doFail "Missing implementation on List comparison for <=")
            <| ( ((,) <$> expectDictionary l' <*> expectDictionary r') >> doFail "Missing implementation on List comparison for <=")
            :| []
        ; return (BoolVal x, a)
        }
    evaluate (GreaterExpr l r a) = do
        { (l', la) <- evaluate l
        ; (r', ra) <- evaluate r
        ; x <- collectError
            $  ( (>) <$> expectInteger l' <*> expectInteger r')
            <| ( (>) <$> expectFloat l' <*> expectFloat r')
            <| ( (>) <$> (fromIntegral <$> expectInteger l') <*> expectFloat r')
            <| ( (>) <$> expectFloat l' <*> (fromIntegral <$> expectInteger r'))
            <| ( (>) <$> expectString l' <*> expectString r')
            <| ( ((,) <$> expectList l' <*> expectList r') >> doFail "Missing implementation on List comparison for >")
            <| ( ((,) <$> expectDictionary l' <*> expectDictionary r') >> doFail "Missing implementation on List comparison for >")
            :| []
        ; return (BoolVal x, a)
        }
    evaluate (GreaterEqualExpr l r a) = do
        { (l', la) <- evaluate l
        ; (r', ra) <- evaluate r
        ; x <- collectError
            $  ( (>=) <$> expectInteger l' <*> expectInteger r')
            <| ( (>=) <$> expectFloat l' <*> expectFloat r')
            <| ( (>=) <$> (fromIntegral <$> expectInteger l') <*> expectFloat r')
            <| ( (>=) <$> expectFloat l' <*> (fromIntegral <$> expectInteger r'))
            <| ( (>=) <$> expectString l' <*> expectString r')
            <| ( ((,) <$> expectList l' <*> expectList r') >> doFail "Missing implementation on List comparison for >=")
            <| ( ((,) <$> expectDictionary l' <*> expectDictionary r') >> doFail "Missing implementation on List comparison for >=")
            :| []
        ; return (BoolVal x, a)
        }
    evaluate (InExpr l r a) = do
        { (l', la) <- evaluate l
        ; (r', ra) <- evaluate r
        ; x <- collectError
            $  ( (l' `elem`) <$> expectList r')
            <| ( (l' `elem`).(fmap fst) <$> expectDictionary r')
            :| []
        ; return (BoolVal x, a)
        }
    evaluate (NotInExpr l r a) = do
        { (l', la) <- evaluate l
        ; (r', ra) <- evaluate r
        ; x <- collectError
            $  ( (l' `notElem`) <$> expectList r')
            <| ( (l' `notElem`).(fmap fst) <$> expectDictionary r')
            :| []
        ; return (BoolVal x, a)
        }
    evaluate (IsExpr l r a) = doFail $ "The \"is\" operator is not supported so far!"
    evaluate (IsNotExpr l r a) = doFail $ "The \"is not\" operator is not supported so far!"
    evaluate (AndExpr l r a)
        =   (\l' r' -> (BoolVal $ (toBool $ fst l') && (toBool $ fst r'), a))
        <$> evaluate l
        <*> evaluate r
    evaluate (OrExpr l r a)
        =   (\l' r' -> (BoolVal $ (toBool $ fst l') || (toBool $ fst r'), a))
        <$> evaluate l
        <*> evaluate r
    evaluate (SliceExpr f l i e a) = doFail $ "Slice is not supported so far!"
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
        ; cond <- collectError
            $  ( False           <$  expectNone c')
            <| ( id              <$> expectBool c')
            <| ( (0/=)           <$> expectInteger c')
            <| ( (0/=)           <$> expectFloat c')
            <| ( not.null        <$> expectString c')
            <| ( not.null        <$> expectList c')
            <| ( not.null        <$> expectObject c')
            :| []
        ; fmap (\x -> (fst x,a)) $ case cond of
            { True -> evaluate p
            ; False -> evaluate n
            }
        }
    evaluate (CallExpr ps c a) = do
        { (c', ca) <- evaluate c
        ; xs <- mapM (evaluate . snd) ps
--        ; res <- collectError
-- $  do { f <- expectFunction c'
--       ; case callFunction f (fmap fst xs) [] of
--         { Left msgs -> return NoneVal
--         ; Right ev ->  case runIdentity $ runException $ runResolverT ev [] of
--             { Left msg -> return NoneVal
--             ; Right x -> return x
--             }
--         }
--       }
        ; res <- do { b <- expectBuildin c'
                    ; return $ case runBuildin b (fmap fst xs) [] of
                        { Left errs -> NoneVal
                        ; Right y -> y
                        }
                    }
        ; return (res, a)
        }
    evaluate (LambdaExpr ns b a) = doFail "Lambda expression is not supported so far"
    evaluate (ComposeExpr x f a) = doFail "Compose expression is not supported so far"

