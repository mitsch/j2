{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Value ( Value(..)
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
              , testValue
              , asNone
              , asBool
              , asInteger
              , asFloat
              , asString
              , asList
              , asDictionary
              , asObject
              , asFunction
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
    ) where

import qualified Control.Monad.Fail as F
import Data.Ratio
import Data.List ( intercalate )

data Value = NoneVal
           | BoolVal Bool
           | IntegerVal Integer
           | FloatVal Float
           | StringVal [Char]
           | ListVal [Value]
           | ObjectVal [([Char], Value)]
           | DictionaryVal [(Value, Value)]
           | FunctionVal [Char] ([Value] -> Either [[Char]] Value)
           | MacroVal [Char] ([Value] -> Either [[Char]] [[Char]])

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

class (F.MonadFail n) => FromValue n a where
    expectNone :: a -> n ()
    expectBool :: a -> n Bool
    expectInteger :: a -> n Integer
    expectFloat :: a -> n Float
    expectString :: a -> n [Char]
    expectList :: a -> n [a]
    expectDictionary :: a -> n [(a, a)]
    expectObject :: a -> n [([Char], a)]
    expectFunction :: a -> n ([Value] -> Either [[Char]] Value)
    expectMacro :: a -> n ([Value] -> Either [[Char]] [[Char]])
    testValue :: a -> Bool

asNone :: (FromValue Maybe a) => a -> Maybe ()
asNone = expectNone

asBool :: (FromValue Maybe a) => a -> Maybe Bool
asBool = expectBool

asInteger :: (FromValue Maybe a) => a -> Maybe Integer
asInteger = expectInteger

asFloat :: (FromValue Maybe a) => a -> Maybe Float
asFloat = expectFloat

asString :: (FromValue Maybe a) => a -> Maybe [Char]
asString = expectString

asList :: (FromValue Maybe a) => a -> Maybe [a]
asList = expectList

asObject :: (FromValue Maybe a) => a -> Maybe [([Char], a)]
asObject = expectObject

asDictionary :: (FromValue Maybe a) => a -> Maybe [(a, a)]
asDictionary = expectDictionary

asFunction :: (FromValue Maybe a) => a -> Maybe ([Value] -> Either [[Char]] Value)
asFunction = expectFunction

asMacro :: (FromValue Maybe a) => a -> Maybe ([Value] -> Either [[Char]] [[Char]])
asMacro = expectMacro


class ToValue a where
    noneVal :: a
    boolVal :: Bool -> a
    integerVal :: Integer -> a
    floatVal :: Float -> a
    stringVal :: [Char] -> a
    listVal :: [a] -> a
    objectVal :: [([Char], a)] -> a
    dictionaryVal :: [(a, a)] -> a
    functionVal :: [Char] -> ([a] -> Either [[Char]] a) -> a
    macroVal :: [Char] -> ([a] -> Either [[Char]] [[Char]]) -> a



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
printCompact (FunctionVal n x) = "@" ++ n
printCompact (MacroVal n x) = "@" ++ n


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
printPretty (FunctionVal n x) = ["@" ++ n]
printPretty (MacroVal n x) = ["@" ++ n]

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

instance (F.MonadFail n) => FromValue n Value where
    expectNone NoneVal = return ()
    expectNone x = fail $ "Expected None but got " ++ show (typeOf x)
    expectBool (BoolVal x) = return x
    expectBool x = fail $ "Expected Bool but got " ++ show (typeOf x)
    expectInteger (IntegerVal x) = return x
    expectInteger x = fail $ "Expected Integer but got " ++ show (typeOf x)
    expectFloat (FloatVal x) = return x
    expectFloat x = fail $ "Expected Float but got " ++ show (typeOf x)
    expectString (StringVal x) = return x
    expectString x = fail $ "Expected String but got " ++ show (typeOf x)
    expectList (ListVal x) = return x
    expectList x = fail $ "Expected List but got " ++ show (typeOf x)
    expectDictionary (DictionaryVal x) = return x
    expectDictionary x = fail $ "Expected Dictionary but got " ++ show (typeOf x)
    expectObject (ObjectVal x) = return x
    expectObject x = fail $ "Expected Object but got " ++ show (typeOf x)
    expectFunction (FunctionVal _ x) = return x
    expectFunction x = fail $ "Expected Function but got " ++ show (typeOf x)
    expectMacro (MacroVal _ x) = return x
    expectMacro x = fail $ "Expected Macro but got " ++ show (typeOf x)
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


instance ToValue (Value) where
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
