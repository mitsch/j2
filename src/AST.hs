{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module AST ( Expression(..)
           , Statement(..)
           , IfStatement(..)
           , ForStatement(..)
           , MacroStatement(..)
           , CallStatement(..)
           , BlockStatement(..)
           ) where

import Control.Applicative (Alternative, (<|>), empty)
import qualified Control.Monad.Fail as F
import Value
    ( expectNone
    , expectBool
    , expectInteger
    , expectFloat
    , expectString
    , expectList
    , expectDictionary
    , expectObject
    , expectFunction
    , FromValue
    , ToValue
    , noneVal
    , boolVal
    , integerVal
    , floatVal
    , stringVal
    , listVal
    , dictionaryVal
    , objectVal
    , functionVal
    , Value(..)
    )
import Data.Ratio (numerator, (%))
import Resolver (MonadResolver, withNames, resolveName)
import Data.Maybe (listToMaybe)

-- TODO should be deprecated:
anyOf :: Alternative f => [f a] -> f a
anyOf [] = empty
anyOf (x:xs) = x <|> anyOf xs


class Tagged a t where
    toTag :: a -> t


data Expression a
    = NoneExpr a
    | BoolExpr Bool a
    | IntegerExpr Integer a
    | NumberExpr Float a
    | StringExpr[Char] a
    | SymbolExpr [Char] a
    | ListExpr [Expression a] a
    | DictionaryExpr [(Expression a, Expression a)] a
    | ObjectExpr [([Char], Expression a)] a
    | NegateExpr (Expression a) a
    | ComplementExpr  (Expression a) a
    | MemberExpr [Char] (Expression a) a
    | IndexExpr (Expression a) (Expression a) a
    | AddExpr (Expression a) (Expression a) a
    | SubtractExpr (Expression a) (Expression a) a
    | MultiplyExpr (Expression a) (Expression a) a
    | DivideExpr (Expression a) (Expression a) a
    | IntegralDivideExpr (Expression a) (Expression a) a
    | ModuloExpr (Expression a) (Expression a) a
    | SameExpr (Expression a) (Expression a) a
    | NotSameExpr (Expression a) (Expression a) a
    | LessExpr (Expression a) (Expression a) a
    | LessEqualExpr (Expression a) (Expression a) a
    | GreaterExpr (Expression a) (Expression a) a
    | GreaterEqualExpr (Expression a) (Expression a) a
    | InExpr (Expression a) (Expression a) a
    | NotInExpr (Expression a) (Expression a) a
    | IsExpr (Expression a) (Expression a) a
    | IsNotExpr (Expression a) (Expression a) a
    | AndExpr (Expression a) (Expression a) a
    | OrExpr (Expression a) (Expression a) a
    | SliceExpr (Maybe (Expression a)) (Maybe (Expression a)) (Maybe (Expression a)) (Expression a) a
    | TernaryExpr (Expression a) (Expression a) (Expression a) a
    | CallExpr [(Maybe [Char], Expression a)] (Expression a) a
    | LambdaExpr [[Char]] (Expression a) a
    | ComposeExpr (Expression a) (Expression a) a

instance Functor (Expression) where
    fmap f (NoneExpr a) = NoneExpr (f a)
    fmap f (BoolExpr x a) = BoolExpr x (f a)
    fmap f (IntegerExpr x a) = IntegerExpr x (f a)
    fmap f (NumberExpr x a) = NumberExpr x (f a)
    fmap f (StringExpr x a) = StringExpr x (f a)
    fmap f (SymbolExpr x a) = SymbolExpr x (f a)
    fmap f (ListExpr xs a) = ListExpr (fmap (fmap f) xs) (f a)
    fmap f (DictionaryExpr xs a) = DictionaryExpr (fmap (\(k,v) -> (fmap f k, fmap f v)) xs) (f a)
    fmap f (ObjectExpr xs a) = ObjectExpr (fmap (\(k,v) -> (k, fmap f v)) xs) (f a)
    fmap f (NegateExpr x a) = NegateExpr (fmap f x) (f a)
    fmap f (ComplementExpr x a) = ComplementExpr (fmap f x) (f a)
    fmap f (MemberExpr n x a) = MemberExpr n (fmap f x) (f a)
    fmap f (IndexExpr i x a) = IndexExpr (fmap f i) (fmap f x) (f a)
    fmap f (AddExpr x y a) = AddExpr (fmap f x) (fmap f y) (f a)
    fmap f (SubtractExpr x y a) = SubtractExpr (fmap f x) (fmap f y) (f a)
    fmap f (MultiplyExpr x y a) = MultiplyExpr (fmap f x) (fmap f y) (f a)
    fmap f (DivideExpr x y a) = DivideExpr (fmap f x) (fmap f y) (f a)
    fmap f (IntegralDivideExpr x y a) = IntegralDivideExpr (fmap f x) (fmap f y) (f a)
    fmap f (ModuloExpr x y a) = ModuloExpr (fmap f x) (fmap f y) (f a)
    fmap f (SameExpr x y a) = SameExpr (fmap f x) (fmap f y) (f a)
    fmap f (NotSameExpr x y a) = NotSameExpr (fmap f x) (fmap f y) (f a)
    fmap f (LessExpr x y a) = LessExpr (fmap f x) (fmap f y) (f a)
    fmap f (LessEqualExpr x y a) = LessEqualExpr (fmap f x) (fmap f y) (f a)
    fmap f (GreaterExpr x y a) = GreaterExpr (fmap f x) (fmap f y) (f a)
    fmap f (GreaterEqualExpr x y a) = GreaterEqualExpr (fmap f x) (fmap f y) (f a)
    fmap f (InExpr x y a) = InExpr (fmap f x) (fmap f y) (f a)
    fmap f (NotInExpr x y a) = NotInExpr (fmap f x) (fmap f y) (f a)
    fmap f (IsExpr x y a) = IsExpr (fmap f x) (fmap f y) (f a)
    fmap f (IsNotExpr x y a) = IsNotExpr (fmap f x) (fmap f y) (f a)
    fmap f (AndExpr x y a) = AndExpr (fmap f x) (fmap f y) (f a)
    fmap f (OrExpr x y a) = OrExpr (fmap f x) (fmap f y) (f a)
    fmap f (SliceExpr u v w x a) = SliceExpr (fmap (fmap f) u) (fmap (fmap f) v) (fmap (fmap f) w) (fmap f x) (f a)
    fmap f (TernaryExpr c p n a) = TernaryExpr (fmap f c) (fmap f p) (fmap f n) (f a)
    fmap f (CallExpr as c a) = CallExpr (flip fmap as $ \(l,x) -> (l, fmap f x)) (fmap f c) (f a)
    fmap f (LambdaExpr ns b a) = LambdaExpr ns (fmap f b) (f a)
    fmap f (ComposeExpr x y a) = ComposeExpr (fmap f x) (fmap f y) (f a)

instance Tagged (Expression a) a where
    toTag (NoneExpr a) = a
    toTag (BoolExpr _ a) = a
    toTag (IntegerExpr _ a) = a
    toTag (NumberExpr _ a) = a
    toTag (StringExpr _ a) = a
    toTag (SymbolExpr _ a) = a
    toTag (ListExpr _ a) = a
    toTag (DictionaryExpr _ a) = a
    toTag (ObjectExpr _ a) = a
    toTag (NegateExpr _ a) = a
    toTag (ComplementExpr _ a) = a
    toTag (MemberExpr _ _ a) = a
    toTag (IndexExpr _ _  a) = a
    toTag (AddExpr _ _ a) = a
    toTag (SubtractExpr _ _ a) = a
    toTag (MultiplyExpr _ _ a) = a
    toTag (DivideExpr _ _ a) = a
    toTag (IntegralDivideExpr _ _ a) = a
    toTag (ModuloExpr _ _ a) = a
    toTag (SameExpr _ _ a) = a
    toTag (NotSameExpr _ _ a) = a
    toTag (LessExpr _ _ a) = a
    toTag (LessEqualExpr _ _ a) = a
    toTag (GreaterExpr _ _ a) = a
    toTag (GreaterEqualExpr _ _ a) = a
    toTag (InExpr _ _ a) = a
    toTag (NotInExpr _ _ a) = a
    toTag (IsExpr _ _ a) = a
    toTag (IsNotExpr _ _ a) = a
    toTag (AndExpr _ _ a) = a
    toTag (OrExpr _ _ a) = a
    toTag (SliceExpr _ _ _ _ a) = a
    toTag (TernaryExpr _ _ _ a) = a
    toTag (CallExpr _ _ a) = a
    toTag (LambdaExpr _ _ a) = a
    toTag (ComposeExpr _ _ a) = a

data ForStatement a = ForStatement
    { forSymbols :: [[Char]]
    , forRange :: Expression a
    , forFilter :: Maybe (Expression a)
    , forRecursion :: Bool
    , forBody :: [Statement a]
    , forEmptyBody :: Maybe ([Statement a], a)
    }

instance Functor (ForStatement) where
    fmap f x = x { forRange = fmap f $ forRange x
                 , forFilter = fmap (fmap f) $ forFilter x
                 , forBody = fmap (fmap f) $ forBody x
                 , forEmptyBody = fmap (\(a',b') -> (fmap (fmap f) a', f b')) $ forEmptyBody x
                 }

data IfStatement a = IfStatement
    { ifBranch :: (Expression a, [Statement a], a)
    , elifBranches :: [(Expression a, [Statement a], a)]
    , elseBranch :: Maybe ([Statement a], a)
    }

instance Functor (IfStatement) where
    fmap f x = let g (a, b, c) = (fmap f a, fmap (fmap f) b, f c)
                   h (a, c) = (fmap (fmap f) a, f c)
               in x { ifBranch = g $ ifBranch x
                    , elifBranches = fmap g $ elifBranches x
                    , elseBranch = fmap h $ elseBranch x}

data MacroStatement a = MacroStatement
    { macroName :: [Char]
    , macroArguments :: [[Char]]
    , macroBody :: [Statement a]
    }

instance Functor (MacroStatement) where
    fmap f x = x { macroBody = fmap (fmap f) $ macroBody x}

data CallStatement a = CallStatement
    { callArguments :: Maybe [[Char]]
    , callName :: [Char]
    , callParameters :: [Expression a]
    , callInput :: [Statement a]
    }

instance Functor (CallStatement) where
    fmap f x = x { callParameters = fmap (fmap f) $ callParameters x
                 , callInput = fmap (fmap f) $ callInput x
                 }

data BlockStatement a = BlockStatement
    { blockName :: [Char]
    , blockScoped :: Bool
    , blockRequired :: Bool
    , blockBody :: [Statement a]
    }

instance Functor (BlockStatement) where
    fmap f x = x { blockBody = fmap (fmap f) $ blockBody x }

data Statement a
    = LiteralStmt [Char] a
    | InterpolationStmt (Expression a) a -- {{ expression }}
    | CommentStmt [Char] a -- {# some comment text #}
    | LineBreakStmt [Char] a -- newline
    | IndentationStmt [Char] a -- nonempty space prefix
    | ForStmt (ForStatement a) a
    | IfStmt (IfStatement a) a
    | MacroStmt (MacroStatement a) a
    | CallStmt (CallStatement a) a
    | FilterStmt [Char] [Statement a] a
    | ExprSetStmt [[Char]] (Expression a) [Statement a] a
    | BlockSetStmt [Char] [Statement a] [Statement a] a
    | IncludeStmt (Expression a) Bool (Maybe Bool) a
    | ImportStmt (Expression a) [Char] a
    | QualifiedImportStmt (Expression a) [([Char], Maybe [Char])] a
    | RawStmt [Char] a
    | BlockStmt (BlockStatement a) a

instance Functor (Statement) where
    fmap f (LiteralStmt t a) = LiteralStmt t (f a)
    fmap f (InterpolationStmt e a) = InterpolationStmt (fmap f e) (f a)
    fmap f (CommentStmt t a) = CommentStmt t (f a)
    fmap f (LineBreakStmt t a) = LineBreakStmt t (f a)
    fmap f (IndentationStmt t a) = IndentationStmt t (f a)
    fmap f (ForStmt x a) = ForStmt (fmap f x) (f a)
    fmap f (IfStmt x a) = IfStmt (fmap f x) (f a)
    fmap f (MacroStmt x a) = MacroStmt (fmap f x) (f a)
    fmap f (CallStmt x a) = CallStmt (fmap f x) (f a)
    fmap f (FilterStmt n xs a) = FilterStmt n (fmap (fmap f) xs) (f a)
    fmap f (ExprSetStmt ns e zs a) = ExprSetStmt ns (fmap f e) (fmap (fmap f) zs) (f a)
    fmap f (BlockSetStmt n xs zs a) = BlockSetStmt n (fmap (fmap f) xs) (fmap (fmap f) zs) (f a)
    fmap f (IncludeStmt x b c a) = IncludeStmt (fmap f x) b c (f a)
    fmap f (ImportStmt o n a) = ImportStmt (fmap f o) n (f a)
    fmap f (QualifiedImportStmt o ns a) = QualifiedImportStmt (fmap f o) ns (f a)
    fmap f (RawStmt x a) = RawStmt x (f a)
    fmap f (BlockStmt x a) = BlockStmt (fmap f x) (f a)


