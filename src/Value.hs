{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Value ( Value(..)
             , Function (..)
             , Type(..)
             , ToValue
             , FromValue
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
             , noneVal
             , boolVal
             , integerVal
             , floatVal
             , stringVal
             , listVal
             , dictionaryVal
             , objectVal
             , functionVal
             , printCompact
             , printPretty
             , typeOf
             , toBool
    ) where

import qualified Control.Monad.Fail as F
import Data.Ratio
import Data.List ( intercalate )
import Evaluation ( Evaluation(..) )
import Location ( Location(..) )
import Error ( Error(..), ExceptionT(..), Exception(..) )
import Function ( Function(..) )
import Buildin ( Buildin(..) )
import Control.Monad.Identity ( Identity )
import Resolver ( ResolverT(..) )


data Value = NoneVal
           | BoolVal Bool
           | IntegerVal Integer
           | FloatVal Float
           | StringVal [Char]
           | ListVal [Value]
           | ObjectVal [([Char], Value)]
           | DictionaryVal [(Value, Value)]
           | FunctionVal [Char] (Function (ResolverT Value (Exception ())) Value)
           | BuildinVal [Char] (Buildin Value Value)
           | MacroVal Location ([Value] -> Either [[Char]] [[Char]])

data Type = NoneType
          | BoolType
          | IntegerType
          | FloatType
          | StringType
          | ListType
          | DictionaryType
          | ObjectType
          | FunctionType
          | MacroType

instance Show Type where
    show NoneType       = "None"
    show BoolType       = "Bool"
    show IntegerType    = "Integer"
    show FloatType      = "Float"
    show StringType     = "String"
    show ListType       = "List"
    show DictionaryType = "Dictionary"
    show ObjectType     = "Object"
    show FunctionType   = "Function"
    show MacroType      = "Macro"

class FromValue n a where
    expectNone :: a -> n ()
    expectBool :: a -> n Bool
    expectInteger :: a -> n Integer
    expectFloat :: a -> n Float
    expectString :: a -> n [Char]
    expectList :: a -> n [a]
    expectDictionary :: a -> n [(a, a)]
    expectObject :: a -> n [([Char], a)]
    expectFunction :: a -> n (Function (ResolverT Value (Exception ())) a)
    expectBuildin :: a -> n (Buildin a a)
    expectMacro :: a -> n ([a] -> Either [[Char]] [[Char]])
    testValue :: a -> Bool


class ToValue m a where
    noneVal :: a
    boolVal :: Bool -> a
    integerVal :: Integer -> a
    floatVal :: Float -> a
    stringVal :: [Char] -> a
    listVal :: [a] -> a
    objectVal :: [([Char], a)] -> a
    dictionaryVal :: [(a, a)] -> a
    functionVal :: [Char] -> (Function (ResolverT Value (Exception ())) a) -> a
    macroVal :: Location -> ([a] -> Either [[Char]] [[Char]]) -> a



printCompact :: Value -> [Char]
printCompact NoneVal = "None"
printCompact (BoolVal True) = "True"
printCompact (BoolVal False) = "False"
printCompact (IntegerVal x) = show x
printCompact (FloatVal x) = show x
printCompact (StringVal x) = show x
printCompact (ListVal xs) = "[" ++ (intercalate "," $ fmap f xs) ++ "]"
    where f = printCompact
printCompact (ObjectVal xs) = "{" ++ (intercalate "," $ fmap f xs) ++ "}"
    where f (k,v) = show k ++ ":" ++ printCompact v
printCompact (DictionaryVal xs) = "{" ++ (intercalate "," $ fmap f xs) ++ "}"
    where f (k,v) = printCompact k ++ ":" ++ printCompact v
printCompact (FunctionVal n _) = "@" ++ (show n)
printCompact (MacroVal n _) = "@" ++ (show n)


joinBC :: [[[Char]]] -> [[Char]]
joinBC [] = []
joinBC (x:[]) = x
joinBC (x1:x2:[]) = (f x1) ++ x2 where
    f [] = error "should never been reached"
    f (z:[]) = [z ++ ","]
    f (z:zs) = z:(f zs)
joinBC (x1:x2:xs) = (f x1) ++ (f x2) ++ (joinBC xs) where
    f [] = error "should never been reached"
    f (z:[]) = [z ++ ","]
    f (z:zs) = z:(f zs)

printPretty :: Value -> [[Char]]
printPretty NoneVal = ["None"]
printPretty (BoolVal True) = ["True"]
printPretty (BoolVal False) = ["False"]
printPretty (IntegerVal x) = [show x]
printPretty (FloatVal x) = [show x]
printPretty (StringVal x) = [show x]
printPretty (ListVal xs) = ["["] ++ (fmap g $ joinBC $ fmap f xs) ++ ["]"]
    where f = printPretty
          g = ("\t" ++)
printPretty (ObjectVal xs) = ["{"] ++ (fmap g $ joinBC $ fmap f xs) ++ ["}"]
    where f (k,v) = let (vh:vt) = printPretty v
                    in (show k ++ ": " ++ vh):(fmap g vt)
          g = ("\t"++)
printPretty (DictionaryVal xs) = ["{"] ++ (fmap g $ joinBC $ fmap f xs) ++ ["}"]
    where f (k,v) = let (vh:vt) = printPretty v
                    in (printCompact k ++ ": " ++ vh):(fmap g vt)
          g = ("\t"++)
printPretty (FunctionVal n _) = ["@" ++ show n]
printPretty (MacroVal n _) = ["@" ++ show n]

isEqual :: Value -> Value -> Bool
isEqual NoneVal NoneVal = True
isEqual (BoolVal l) (BoolVal r) = l == r
isEqual (IntegerVal l) (IntegerVal r) = l == r
isEqual (FloatVal l) (FloatVal r) = l == r
isEqual (StringVal l) (StringVal r) = l == r
isEqual (ListVal l) (ListVal r) = f l r
    where f (l:ls) (r:rs) = isEqual l r && f ls rs
          f [] []         = True
          f _ _           = False
isEqual (DictionaryVal l) (DictionaryVal r) = False
isEqual (ObjectVal l) (ObjectVal r) = False
isEqual (FunctionVal l _) (FunctionVal r _) = l == r
isEqual (MacroVal l _) (MacroVal r _) = l == r
isEqual _ _ = False

instance Eq (Value) where
    (==) = isEqual

typeOf :: Value -> Type
typeOf NoneVal           = NoneType
typeOf (BoolVal _)       = BoolType
typeOf (IntegerVal _)    = IntegerType
typeOf (FloatVal _)      = FloatType
typeOf (StringVal _)     = StringType
typeOf (ListVal _)       = ListType
typeOf (DictionaryVal _) = DictionaryType
typeOf (ObjectVal _)     = ObjectType
typeOf (FunctionVal _ _) = FunctionType
typeOf (MacroVal _ _)    = MacroType

toBool :: Value -> Bool
toBool NoneVal = False
toBool (BoolVal x) = x
toBool (IntegerVal x) = x /= 0
toBool (FloatVal x) = x /= 0
toBool (StringVal x) = not $ null x
toBool (ListVal x) = not $ null x
toBool (DictionaryVal x) = not $ null x
toBool (ObjectVal x) = not $ null x
toBool _ = False



instance (Applicative n, Error t n) => FromValue n Value where
    expectNone NoneVal = pure ()
    expectNone x = throwError ("Expected None but got " ++ show (typeOf x))
    expectBool (BoolVal x) = pure x
    expectBool x = throwError $ "Expected Bool but got " ++ show (typeOf x)
    expectInteger (IntegerVal x) = pure x
    expectInteger x = throwError $ "Expected Integer but got " ++ show (typeOf x)
    expectFloat (FloatVal x) = pure x
    expectFloat x = throwError $ "Expected Float but got " ++ show (typeOf x)
    expectString (StringVal x) = pure x
    expectString x = throwError $ "Expected String but got " ++ show (typeOf x)
    expectList (ListVal x) = pure x
    expectList x = throwError $ "Expected List but got " ++ show (typeOf x)
    expectDictionary (DictionaryVal x) = pure x
    expectDictionary x = throwError $ "Expected Dictionary but got " ++ show (typeOf x)
    expectObject (ObjectVal x) = pure x
    expectObject x = throwError $ "Expected Object but got " ++ show (typeOf x)
    expectFunction (FunctionVal _ x) = pure x
    expectFunction x = throwError $ "Expected Function but got " ++ show (typeOf x)
    expectBuildin (BuildinVal _ x) = pure x
    expectBuildin x = throwError $ "Expected Buildin but got " ++ show (typeOf x)
    expectMacro (MacroVal _ x) = pure x
    expectMacro x = throwError $ "Expected Macro but got " ++ show (typeOf x)
    testValue NoneVal = False
    testValue (BoolVal x) = x
    testValue (IntegerVal x) = x /= 0
    testValue (FloatVal x) = x /= 0
    testValue (StringVal x) = (length x) /= 0
    testValue (ListVal x) = (length x) /= 0
    testValue (DictionaryVal x) = (length x) /= 0
    testValue (ObjectVal x) = (length x) /= 0
    testValue (FunctionVal _ _) = True
    testValue (MacroVal _ _) = True


instance ToValue m Value where
    noneVal       = NoneVal
    boolVal       = BoolVal
    integerVal    = IntegerVal
    floatVal      = FloatVal
    stringVal     = StringVal
    listVal       = ListVal
    dictionaryVal = DictionaryVal
    objectVal     = ObjectVal
    functionVal   = FunctionVal
    macroVal      = MacroVal
