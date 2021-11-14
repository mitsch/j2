{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Value ( Value(..)
              , ToValue
              , FromValue
              , expectNone
              , expectBool
              , expectInteger
              , expectDecimal
              , expectString
              , expectList
              , expectDictionary
              , expectObject
              , expectFunction
              , asNone
              , asBool
              , asInteger
              , asDecimal
              , asString
              , asList
              , asDictionary
              , asObject
              , asFunction
              , noneVal
              , boolVal
              , integerVal
              , decimalVal
              , stringVal
              , listVal
              , dictionaryVal
              , objectVal
              , functionVal
    ) where

import qualified Control.Monad.Fail as F
import Data.Ratio
import Data.List ( intercalate )

data Type = NoneType
          | BoolType
          | IntegerType
          | DecimalType
          | StringType
          | ListType
          | DictionaryType
          | ObjectType
          | FunctionType

instance Show Type where
    show NoneType       = "None"
    show BoolType       = "Bool"
    show IntegerType    = "Integer"
    show DecimalType    = "Decimal"
    show StringType     = "String"
    show ListType       = "List"
    show DictionaryType = "Dictionary"
    show ObjectType     = "Object"
    show FunctionType   = "Function"

class (F.MonadFail n) => FromValue n a where
    expectNone :: a -> n ()
    expectBool :: a -> n Bool
    expectInteger :: a -> n Integer
    expectDecimal :: a -> n Rational
    expectString :: a -> n [Char]
    expectList :: a -> n [a]
    expectDictionary :: a -> n [(a, a)]
    expectObject :: a -> n [([Char], a)]
    expectFunction :: a -> n [Char]

asNone :: (FromValue Maybe a) => a -> Maybe ()
asNone = expectNone

asBool :: (FromValue Maybe a) => a -> Maybe Bool
asBool = expectBool

asInteger :: (FromValue Maybe a) => a -> Maybe Integer
asInteger = expectInteger

asDecimal :: (FromValue Maybe a) => a -> Maybe Rational
asDecimal = expectDecimal

asString :: (FromValue Maybe a) => a -> Maybe [Char]
asString = expectString

asList :: (FromValue Maybe a) => a -> Maybe [a]
asList = expectList

asObject :: (FromValue Maybe a) => a -> Maybe [([Char], a)]
asObject = expectObject

asDictionary :: (FromValue Maybe a) => a -> Maybe [(a, a)]
asDictionary = expectDictionary

asFunction :: (FromValue Maybe a) => a -> Maybe [Char]
asFunction = expectFunction

class ToValue a where
    noneVal :: a
    boolVal :: Bool -> a
    integerVal :: Integer -> a
    decimalVal :: Rational -> a
    stringVal :: [Char] -> a
    listVal :: [a] -> a
    objectVal :: [([Char], a)] -> a
    dictionaryVal :: [(a, a)] -> a
    functionVal :: [Char] -> a


data Value = NoneVal
           | BoolVal Bool
           | IntegerVal Integer
           | DecimalVal Rational
           | StringVal [Char]
           | ListVal [Value]
           | ObjectVal [([Char], Value)]
           | DictionaryVal [(Value, Value)]
           | FunctionVal [Char]

printCompact :: Value -> [Char]
printCompact NoneVal = "None"
printCompact (BoolVal True) = "True"
printCompact (BoolVal False) = "False"
printCompact (IntegerVal x) = show x
printCompact (DecimalVal x) = show $ n / d
    where n = fromIntegral $ numerator x
          d = fromIntegral $ denominator x
printCompact (StringVal x) = show x
printCompact (ListVal xs) = "[" ++ (intercalate "," $ fmap f xs) ++ "]"
    where f = printCompact
printCompact (ObjectVal xs) = "{" ++ (intercalate "," $ fmap f xs) ++ "}"
    where f (k,v) = show k ++ ":" ++ printCompact v
printCompact (DictionaryVal xs) = "{" ++ (intercalate "," $ fmap f xs) ++ "}"
    where f (k,v) = printCompact k ++ ":" ++ printCompact v
printCompact (FunctionVal x) = "@" ++ x


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
printPretty (DecimalVal x) = [show $ n / d]
    where n = fromIntegral $ numerator x
          d = fromIntegral $ denominator x
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
printPretty (FunctionVal x) = ["@" ++ x]

isEqual :: Value -> Value -> Bool
isEqual NoneVal NoneVal = True
isEqual (BoolVal l) (BoolVal r) = l == r
isEqual (IntegerVal l) (IntegerVal r) = l == r
isEqual (DecimalVal l) (DecimalVal r) = l == r
isEqual (StringVal l) (StringVal r) = l == r
isEqual (ListVal l) (ListVal r) = f l r
    where f (l:ls) (r:rs) = isEqual l r && f ls rs
          f [] []         = True
          f _ _           = False
isEqual (DictionaryVal l) (DictionaryVal r) = False
isEqual (ObjectVal l) (ObjectVal r) = False
isEqual (FunctionVal l) (FunctionVal r) = l == r

instance Eq (Value) where
    (==) = isEqual

typeOf :: Value -> Type
typeOf NoneVal           = NoneType
typeOf (BoolVal _)       = BoolType
typeOf (IntegerVal _)    = IntegerType
typeOf (DecimalVal _)    = DecimalType
typeOf (StringVal _)     = StringType
typeOf (ListVal _)       = ListType
typeOf (DictionaryVal _) = DictionaryType
typeOf (ObjectVal _)     = ObjectType
typeOf (FunctionVal _)   = FunctionType

instance (F.MonadFail n) => FromValue n Value where
    expectNone NoneVal = return ()
    expectNone x = fail $ "Expected None but got " ++ show (typeOf x)
    expectBool (BoolVal x) = return x
    expectBool x = fail $ "Expected Bool but got " ++ show (typeOf x)
    expectInteger (IntegerVal x) = return x
    expectInteger x = fail $ "Expected Integer but got " ++ show (typeOf x)
    expectDecimal (DecimalVal x) = return x
    expectDecimal x = fail $ "Expected Decimal but got " ++ show (typeOf x)
    expectString (StringVal x) = return x
    expectString x = fail $ "Expected String but got " ++ show (typeOf x)
    expectList (ListVal x) = return x
    expectList x = fail $ "Expected List but got " ++ show (typeOf x)
    expectDictionary (DictionaryVal x) = return x
    expectDictionary x = fail $ "Expected Dictionary but got " ++ show (typeOf x)
    expectObject (ObjectVal x) = return x
    expectObject x = fail $ "Expected Object but got " ++ show (typeOf x)
    expectFunction (FunctionVal x) = return x
    expectFunction x = fail $ "Expected Function but got " ++ show (typeOf x)


instance ToValue (Value) where
    noneVal = NoneVal
    boolVal = BoolVal
    integerVal = IntegerVal
    decimalVal = DecimalVal
    stringVal = StringVal
    listVal = ListVal
    dictionaryVal = DictionaryVal
    objectVal = ObjectVal
    functionVal = FunctionVal
