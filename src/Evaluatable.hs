{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Evaluatable ( Evaluatable, evaluate) where

import AST ( Expression(..), Statement(..) )
import Value ( Value(..)
             , Function(..)
             , expectNone
             , expectBool
             , expectInteger
             , expectFloat
             , expectString
             , expectList
             , expectDictionary
             , expectObject
             , expectFunction
             , expectBuildin
             , testValue
             , toBool
             , noneVal
             , boolVal
             , integerVal
             , floatVal
             , stringVal
             , listVal
             , dictionaryVal
             , objectVal
             , functionVal
             , FromValue
             )
import qualified Control.Monad.Fail as F
import Control.Applicative ( Alternative, empty, (<|>) )
import Data.Ratio ( numerator )
import Data.Maybe ( listToMaybe )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Evaluation ( Evaluation(..) )
import Error ( Error, collectError, throwError, Exception(..), ExceptionT(..) )
import Data.List.NonEmpty ( (<|), NonEmpty( (:|) ) )
import Control.Monad.Identity ( Identity(..) )
import Function ( callFunction )
import Buildin ( runBuildin )
import Resolver ( MonadResolver, resolveName, ResolverT(..), liftResolverT )



class Evaluatable e m where
    evaluate :: e a -> m (Value, a)


instance ( Monad m
         , F.MonadFail m
         , MonadResolver Value m
         , MonadIO m
         , Error t m
         ) => Evaluatable Expression m where
    evaluate (NoneExpr a) = return (noneVal, a)
    evaluate (BoolExpr b a) = return (boolVal b, a)
    evaluate (IntegerExpr i a) = return (integerVal i, a)
    evaluate (NumberExpr f a) = return (floatVal f, a)
    evaluate (StringExpr s a) = return (stringVal s, a)
    evaluate (SymbolExpr s a) = fmap (\v -> (v,a)) $ resolveName s
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
        ; n <- collectError
                $  ((integerVal . negate) <$> expectInteger e')
                <| ((floatVal . negate)   <$> expectFloat e')
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
                <| (False   <$  expectFunction e')
                :| []
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
        ; x <- collectError
                $  do{ ls <- expectList e'
                     ; n <- expectInteger i'
                     ; case listToMaybe $ drop (fromEnum n) ls of
                        { Just x -> return x
                        ; Nothing -> F.fail $ "Cannot get index " ++ show n
                        }
                     }
                <| do{ os <- expectDictionary e'
                     ; case lookup i' os of
                        { Just x -> return x
                          --TODO fix formating of i'
                        ; Nothing -> F.fail $ "Cannot find " ++ "value of i>" ++ " as key"
                        }
                     }
                :| []
        ; return (x, a)
        }
    evaluate (AddExpr l r a) = do
        { (l', la) <- evaluate l
        ; (r', ra) <- evaluate r
        ; x <- collectError
                $  ( (integerVal.).(+)
                        <$> expectInteger l'
                        <*> expectInteger r')
                <| ( (floatVal.).(+)
                        <$> expectFloat l'
                        <*> expectFloat r')
                <| ( (floatVal.).(+)
                        <$> (fromIntegral <$> expectInteger l')
                        <*> expectFloat r')
                <| ( (floatVal.).(+)
                        <$> expectFloat l'
                        <*> (fromIntegral <$> expectInteger r'))
                :| []
        ; return (x, a)
        }
    evaluate (SubtractExpr l r a) = do
        { (l', la) <- evaluate l
        ; (r', ra) <- evaluate r
        ; x <- collectError
            $  ( (integerVal.).(-)
                <$> expectInteger l'
                <*> expectInteger r')
            <| ( (floatVal.).(-)
                <$> expectFloat l'
                <*> expectFloat r')
            <| ( (floatVal.).(-)
                <$> (fromIntegral <$> expectInteger l')
                <*> expectFloat r')
            <| ( (floatVal.).(-)
                <$> expectFloat l'
                <*> (fromIntegral <$> expectInteger r'))
            :| []
        ; return (x, a)
        }
    evaluate (MultiplyExpr l r a) = do
        { (l', la) <- evaluate l
        ; (r', ra) <- evaluate r
        ; x <- collectError
            $ ( (integerVal.).(*)
                <$> expectInteger l'
                <*> expectInteger r')
            <| ( (floatVal.).(*)
                <$> expectFloat l'
                <*> expectFloat r')
            <| ( (floatVal.).(*)
                <$> (fromIntegral <$> expectInteger l')
                <*> expectFloat r')
            <| ( (floatVal.).(*)
                <$> expectFloat l'
                <*> (fromIntegral <$> expectInteger r'))
            :| []
        ; return (x, a)
        }
    evaluate (DivideExpr l r a) = do
        { (l', la) <- evaluate l
        ; (r', ra) <- evaluate r
        ;       (\lh rh -> (floatVal $ lh / rh, a))
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
            $  ( (integerVal.).div
                    <$> expectInteger l'
                    <*> expectInteger r')
            <| ( ((integerVal . floor) .).(/)
                    <$> (fromIntegral <$> expectInteger l')
                    <*> expectFloat r')
            <| ( ((integerVal . floor) .).(/)
                    <$> expectFloat l'
                    <*> (fromIntegral <$> expectInteger r'))
            <| ( ((integerVal . floor) .).(/)
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
            $  ( (integerVal.).(mod)
                    <$> expectInteger l'
                    <*> expectInteger r')
            <| ( (floatVal.).(\a b -> a - (fromIntegral $ floor $ a / b) * b)
                    <$> expectFloat l'
                    <*> (fromIntegral <$> expectInteger r'))
            <| ( (floatVal.).(\a b -> a - (fromIntegral $ floor $ a / b) * b)
                    <$> (fromIntegral <$> expectInteger l')
                    <*> expectFloat r')
            <| ( (floatVal.).(\a b -> a - (fromIntegral $ floor $ a / b) * b)
                    <$> expectFloat l'
                    <*> expectFloat r')
            :| []
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
        ; x <- collectError
            $  ( (<) <$> expectInteger l' <*> expectInteger r')
            <| ( (<) <$> expectFloat l' <*> expectFloat r')
            <| ( (<) <$> (fromIntegral <$> expectInteger l') <*> expectFloat r')
            <| ( (<) <$> expectFloat l' <*> (fromIntegral <$> expectInteger r'))
            <| ( (<) <$> expectString l' <*> expectString r')
            <| ( ((,) <$> expectList l' <*> expectList r') >> F.fail "Missing implementation on List comparison for <")
            <| ( ((,) <$> expectDictionary l' <*> expectDictionary r') >> F.fail "Missing implementation on List comparison for <")
            :| []
        ; return (boolVal x, a)
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
            <| ( ((,) <$> expectList l' <*> expectList r') >> F.fail "Missing implementation on List comparison for <=")
            <| ( ((,) <$> expectDictionary l' <*> expectDictionary r') >> F.fail "Missing implementation on List comparison for <=")
            :| []
        ; return (boolVal x, a)
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
            <| ( ((,) <$> expectList l' <*> expectList r') >> F.fail "Missing implementation on List comparison for >")
            <| ( ((,) <$> expectDictionary l' <*> expectDictionary r') >> F.fail "Missing implementation on List comparison for >")
            :| []
        ; return (boolVal x, a)
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
            <| ( ((,) <$> expectList l' <*> expectList r') >> F.fail "Missing implementation on List comparison for >=")
            <| ( ((,) <$> expectDictionary l' <*> expectDictionary r') >> F.fail "Missing implementation on List comparison for >=")
            :| []
        ; return (boolVal x, a)
        }
    evaluate (InExpr l r a) = do
        { (l', la) <- evaluate l
        ; (r', ra) <- evaluate r
        ; x <- collectError
            $  ( (l' `elem`) <$> expectList r')
            <| ( (l' `elem`).(fmap fst) <$> expectDictionary r')
            :| []
        ; return (boolVal x, a)
        }
    evaluate (NotInExpr l r a) = do
        { (l', la) <- evaluate l
        ; (r', ra) <- evaluate r
        ; x <- collectError
            $  ( (l' `notElem`) <$> expectList r')
            <| ( (l' `notElem`).(fmap fst) <$> expectDictionary r')
            :| []
        ; return (boolVal x, a)
        }
    evaluate (IsExpr l r a) = F.fail $ "The \"is\" operator is not supported so far!"
    evaluate (IsNotExpr l r a) = F.fail $ "The \"is not\" operator is not supported so far!"
    evaluate (AndExpr l r a)
        =   (\l' r' -> (boolVal $ (toBool $ fst l') && (toBool $ fst r'), a))
        <$> evaluate l
        <*> evaluate r
    evaluate (OrExpr l r a)
        =   (\l' r' -> (boolVal $ (toBool $ fst l') || (toBool $ fst r'), a))
        <$> evaluate l
        <*> evaluate r
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
        ; res <- collectError
            $  do { f <- expectFunction c'
                  ; case callFunction f (fmap fst xs) [] of
                    { Left msgs -> return NoneVal
                    ; Right ev ->  case runIdentity $ runException $ runResolverT ev [] of
                        { Left msg -> return NoneVal
                        ; Right x -> return x
                        }
                    }
                  }
            <| do { b <- expectBuildin c'
                  ; return $ case runBuildin b (fmap fst xs) [] of
                    { Left errs -> NoneVal
                    ; Right y -> y
                    }
                  }
            :| []
        ; return (res, a)
        }
    evaluate (LambdaExpr ns b a) = F.fail "Lambda expression is not supported so far"
    evaluate (ComposeExpr x f a) = F.fail "Compose expression is not supported so far"

