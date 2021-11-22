{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Parser   ( expression
                , term
                , baseTemplate
                , childTemplate
                ) where

import Numeric (readHex, readOct)
import Text.Parsec
import Data.Ratio (Rational, (%))
import Data.Char (isSpace)
import AST
    ( Expression(..)
    , Statement(..)
    , IfStatement(..)
    , ForStatement(..)
    , MacroStatement(..)
    , CallStatement(..)
    , BlockStatement(..))
import Control.Applicative (liftA2)


commaList :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
commaList p = (p <* spaces) `sepBy` (char ',' <* spaces)

commaList1 :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
commaList1 p = (p <* spaces) `sepBy1` (char ',' <* spaces)

betweenBrackets :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
betweenBrackets = between (char '[' <* spaces) (char ']')

betweenBraces :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
betweenBraces = between (char '{' <* spaces) (char '}')

betweenParens :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
betweenParens = between (char '(' <* spaces) (char ')')

unaryPrefix :: Stream s m t => ParsecT s u m a
                            -> ParsecT s u m (a -> a)
                            -> ParsecT s u m a
unaryPrefix p pf = flip (foldr ($)) <$> many pf <*> p

unarySuffix :: Stream s m t => ParsecT s u m a
                            -> ParsecT s u m (a -> a)
                            -> ParsecT s u m a
unarySuffix p pf = foldl (flip ($)) <$> p <*> many pf

identifier :: Stream s m Char => ParsecT s u m [Char]
identifier = (:) <$> (char '_' <|> letter) <*> many (char '_' <|> alphaNum)

allKeywords = [ "Null"
              , "True", "False"
              , "and", "or"
              , "if", "elif", "else", "endif"
              , "for", "in", "endfor"
              , ""]

keyword :: Stream s m Char => [Char] -> ParsecT s u m [Char]
keyword n = try (string n <* notFollowedBy (char '_' <|> alphaNum))

operator :: Stream s m Char => [Char] -> ParsecT s u m [Char]
operator n = try (string n <* notFollowedBy (oneOf "+-*/%.=!<>|]}"))

literalString :: Stream s m Char => ParsecT s u m [Char]
literalString = between (char '\"') (char '\"')
              $ many
              $ noneOf "\"\\"
              <|> (char '\\' *> (ucode <|> ocode <|> hcode <|> escaped))
        where ucode = (toEnum.fst.head.readHex) <$> (char 'u' *> count 4 hexDigit)
              ocode = (toEnum.fst.head.readOct) <$> (char 'o' *> count 2 octDigit)
              hcode = (toEnum.fst.head.readHex) <$> (char 'h' *> count 2 hexDigit)
              escaped = choice $ zipWith (\x y -> y <$ char x) esc_from esc_to
              esc_from = "\'\\nrtbf"
              esc_to = "\'\\\n\r\t\b\f"

literalNumber :: Stream s m Char => ParsecT s u m (Either Integer Rational)
literalNumber =   f
              <$> ((toInteger . read) <$> many1 digit)
              <*> optionMaybe (oneOf "eE" *> many1 digit)
              <*> optionMaybe (char '.' *> many1 digit)
        where f a Nothing Nothing = Left a
              f a b c = Right $ g a (maybe 0 (fromIntegral . read) b)
                                    (maybe 0 length b)
                                    (maybe 0 (fromIntegral . read) c)
              g a b c d = toInteger ((a * 10 ^ c + b) * 10 ^ (max 0 $ d - c))
                        % toInteger (10 ^ (max 0 $ c - d))

---

withPos :: Stream s m Char => ParsecT s u m ((SourcePos, SourcePos) -> b)
                           -> ParsecT s u m b
withPos p = (\a f b -> f (a,b)) <$> getPosition <*> p <*> getPosition

---

term :: (Stream s m Char) => ParsecT s u m (Expression (SourcePos, SourcePos))
                          -> ParsecT s u m (Expression (SourcePos, SourcePos))
term p = withPos $ choice
        [ NoneExpr       <$  keyword "None"
        , BoolExpr True  <$  keyword "True"
        , BoolExpr False <$  keyword "False"
        , StringExpr     <$> literalString
        , fromNumber     <$> literalNumber
        , SymbolExpr     <$> identifier
        , ListExpr       <$> betweenBrackets (commaList p)
        , DictionaryExpr <$> betweenBraces (commaList $ ((,) <$> keyField <*> valueField))
        ]
        where fromNumber = either IntegerExpr NumberExpr
              keyField = p <* spaces <* char ':'
              valueField = spaces *> p <* spaces


expression :: (Stream s m Char) => ParsecT s u m (Expression (SourcePos, SourcePos))
expression = (choice
                [ term expression
                , betweenParens $ expression <* spaces
                ])
        `unarySuffix`
                (choice
                [ MemberExpr <$> (operator "." *> identifier)
                , CallExpr <$> (betweenParens $ commaList $ fmap (\v->(Nothing,v)) $ expression <* spaces)
                ])
        `unaryPrefix`
                (choice
                [ NegateExpr <$ operator "-" <* spaces
                , ComplementExpr <$ keyword "not" <* spaces
                ])
        `unarySuffix` ((\x a -> fmap (const a) x) <$ space <* spaces)
        `binaryOpL`
                (choice
                [ MultiplyExpr <$ operator "*" <* spaces
                , IntegralDivideExpr <$ operator "//" <* spaces
                , DivideExpr <$ operator "/" <* spaces
                , ModuloExpr <$ operator "%" <* spaces
                ])
        `binaryOpL`
                (choice
                [ AddExpr <$ operator "+" <* spaces
                , SubtractExpr <$ operator "-" <* spaces
                ])
        `binaryOpL`
                (choice
                [ SameExpr <$ operator "==" <* spaces
                , NotSameExpr <$ operator "!=" <* spaces
                , IsNotExpr <$ keyword "is no" <* spaces
                , IsExpr <$ keyword "is" <* spaces
                , InExpr <$ keyword "in" <* spaces
                , NotInExpr <$ keyword "not in" <* spaces
                , LessEqualExpr <$ operator "<=" <* spaces
                , LessExpr <$ operator "<" <* spaces
                , GreaterEqualExpr <$ operator ">=" <* spaces
                , GreaterExpr <$ operator ">" <* spaces
                ])
        `binaryOpL` (AndExpr <$ keyword "and" <* spaces)
        `binaryOpL` (OrExpr <$ keyword "or" <* spaces)
        `binaryOpL` (ComposeExpr <$ operator "|" <* spaces)
        where unarySuffix :: (Monad m)
                          => ParsecT s u m (Expression (SourcePos, SourcePos))
                          -> ParsecT s u m (Expression (SourcePos, SourcePos) -> (SourcePos, SourcePos) -> Expression (SourcePos, SourcePos))
                          -> ParsecT s u m (Expression (SourcePos, SourcePos)) 
              unarySuffix p pf
                =   (\a -> foldl (\x f -> f x a))
                <$> getPosition
                <*> p
                <*> many ((\f b x a -> f x (a,b))  <$> pf <*> getPosition)
              unaryPrefix :: (Monad m)
                          => ParsecT s u m (Expression (SourcePos, SourcePos))
                          -> ParsecT s u m (Expression (SourcePos, SourcePos) -> (SourcePos, SourcePos) -> Expression (SourcePos, SourcePos))
                          -> ParsecT s u m (Expression (SourcePos, SourcePos))
              unaryPrefix p pf
                =   (\fs x b -> foldr (\f y -> f y b) x fs)
                <$> many ((\a f x b -> f x (a,b)) <$> getPosition <*> pf)
                <*> p
                <*> getPosition
              binaryOpL :: (Monad m)
                        => ParsecT s u m (Expression (SourcePos, SourcePos))
                        -> ParsecT s u m (Expression (SourcePos, SourcePos) -> Expression (SourcePos, SourcePos) -> (SourcePos, SourcePos) -> Expression (SourcePos, SourcePos))
                        -> ParsecT s u m (Expression (SourcePos, SourcePos))
              binaryOpL p pf
                =   (\a l fs -> foldl (\l f -> f l a) l fs)
                <$> getPosition
                <*> p
                <*> many ((\f r b l a -> f l r (a,b)) <$> pf <*> p <*> getPosition)


---

simpleTag :: Stream s m Char => [Char] -> ParsecT s u m ()
simpleTag n = () <$ (try (b *> q *> spaces *> keyword n) *> spaces*> q *> e)
    where q = optional $ oneOf "+-"
          b = string "{%"
          e = string "%}"

fancyTag :: Stream s m Char => [Char] -> ParsecT s u m a -> ParsecT s u m a
fancyTag n p = try (b *> q *> s *> keyword n) *> s *> p <* s <* q <* e
    where q = optional $ oneOf "+-"
          b = string "{%"
          e = string "%}"
          s = spaces

---

statement :: (Stream s m Char) => ParsecT s u m (Statement (SourcePos, SourcePos))
statement = withPos $ choice
    [ fmap IndentationStmt
        $ many1
        $ satisfy
        $ \x -> isSpace x && x `notElem` "\r\n"
    , fmap LineBreakStmt
        $ string "\n" <|> string "\r\n"
    , fmap CommentStmt
        $ (try $ string "{#") *> manyTill anyChar (try $ string "#}")
    , fmap InterpolationStmt
        $ between (try $ string "{{") (string "}}")
        $ between spaces spaces
        $ expression
    , fmap IfStmt
        $   IfStatement
        <$> withPos ((,,) <$> fancyTag "if" expression <*> many statement)
        <*> many (withPos ((,,) <$> fancyTag "elif" expression <*> many statement))
        <*> optionMaybe (withPos ((,) <$> (simpleTag "else" *> many statement)))
        <*  simpleTag "endif"
    , fmap ForStmt
        $ fancyTag "for" (ForStatement
            <$> (identifier <* spaces) `sepBy1` (char ',' <* spaces)
            <*> (keyword "in" *> spaces *> expression)
            <*> optionMaybe (keyword "if" *> spaces *> expression)
            <*> option False (True <$ keyword "recursive"))
        <*> many statement
        <*> optionMaybe (withPos ((,) <$> (simpleTag "else" *> many statement)))
        <*  simpleTag "endfor"
    , fmap MacroStmt
        $ fancyTag "macro" (MacroStatement
            <$> (identifier <* spaces)
            <*> (betweenParens $ commaList $ identifier <* spaces))
        <*> many statement
        <*  simpleTag "endmacro"
    , fmap CallStmt
        $ fancyTag "call" (CallStatement
            <$> optionMaybe (betweenParens $ commaList $ identifier <* spaces)
            <*> (spaces *> identifier <* spaces)
            <*> (betweenParens $ commaList $ expression <* spaces))
        <*> many statement
        <*  simpleTag "endcall"
    , FilterStmt
        <$> fancyTag "filter" identifier
        <*> many statement
        <*  simpleTag "endfilter"
    , fancyTag "set" (choice
        [ do { ns <- try (commaList1 (identifier <* spaces) <* char '=')
             ; spaces
             ; e <- expression
             ; return $ return $ ExprSetStmt ns e
             }
        , do { n <- identifier
             ; spaces
             ; return $ do
                { xs <- many statement
                ; simpleTag "endset"
                ; return $ BlockSetStmt n xs
                }
             }
        ])
        >>= (\f -> liftA2 ($) f (many statement))
    , fancyTag "include" $ IncludeStmt
        <$> (expression <* spaces)
        <*> option False (True <$ keyword "ignore missing" <* spaces)
        <*> optionMaybe (
                (True <$ keyword "with context")
            <|> (False <$ keyword "without context"))
    , fancyTag "import" $ ImportStmt
        <$> (expression <* spaces)
        <*> (keyword "as" *> spaces *> identifier)
    , fancyTag "from" $ QualifiedImportStmt
        <$> (expression <* spaces)
        <*> (keyword "import" *> spaces *> commaList1 ((,) <$> (identifier <* spaces) <*> optionMaybe (keyword "as" *> spaces *> identifier)))
    , liftA2 (\_ x-> RawStmt x) (simpleTag "raw")
        $ manyTill anyChar (simpleTag "endraw")
    , fancyTag "block" ((,,)
            <$> (identifier <* spaces)
            <*> (option False $ True <$ keyword "scoped" <* spaces)
            <*> (option False $ True <$ keyword "required" <* spaces))
        >>= \(a,b,c) ->
                (BlockStmt . (BlockStatement a b c))
            <$> many statement
            <*  fancyTag "endblock" (optional $ string a)
    , fmap LiteralStmt
        $ many1
        $ notFollowedBy
            (   (try $ string "{{")
            <|> (try $ string "{%")
            <|> (try $ string "{#")
            <|> (string "\n")
            <|> (string "\r\n"))
        *> anyChar
    ]

baseTemplate :: (Stream s m Char) => ParsecT s u m [Statement (SourcePos, SourcePos)]
baseTemplate = many statement <* eof

childTemplate :: (Stream s m Char) => ParsecT s u m ([Char], [([Char], [Statement  (SourcePos, SourcePos)])])
childTemplate =   (,)
              <$> fancyTag "extends" literalString
              <*> many ((,)
                    <$> fancyTag "block" identifier
                    <*> many statement
                    <*  simpleTag "endblock")
              <*  eof


