module Parser () where

stringToken :: Stream s m Char => ParsecT s u m [Char]
stringToken = between (char '\"') (char '\"')
            $ many ()

numberToken :: Stream s m Char => ParsecT s u m [Char]

symbolToken :: Stream s m Char => ParsecT s u m [Char]
symbolToken = (:) <$> (char '_' <|> letter) <*> many (char '_' <|> alphaNum)

spaceToken :: Stream s m Char => ParsecT s u m ()
spaceToken = () <$ (space *> spaces)

opToken :: Stream s m Char => ParsecT s u m [Char]
opToken =   string "+"
        <|> string "-"
        <|> string "*"
        <|> try (string "//")
        <|> string "/"
        <|> string "%"
        <|> string "("
        <|> string ")"
        <|> string "["
        <|> string "]"
        <|> string "{"
        <|> string "}"
        <|> string ","
        <|> string ":"
        <|> string "|"
        <|> string "="

data CodeToken = StringToken [Char]
               | NumberToken [Char]
               | SymbolToken [Char]
               | SpaceToken
               | OpToken [Char]

expres

lineBreakToken :: Stream s m Char => ParsecT s u m [Char]
lineBreakToken = string "\n" <|> string "\r\n"

textToken :: Stream s m Char => ParsecT s u m [Char]
textToken =

commentToken :: Stream s m Char => ParsecT s u m [Char]
commentToken = 

-- -------------------------------

ifStmt :: Stream s m Char => ParsecT s u m (Result Bool)
ifStmt = keyword "if" *> spaces *> (expression >>= expectBool)

elifStmt :: Stream s m Char => ParsecT s u m (Result Bool)
elifStmt = keyword "elif" *> spaces *> (expression >>= expectBool)

elseStmt :: Stream s m Char => ParsecT s u m ()
elseStmt = () <$ keyword "else"

endifStmt :: Stream s m Char => ParsecT s u m ()
endifStmt = () <$ keyword "endif"

forStmt :: Stream s m Char => ParsecT s u m ([[Char]], Result Value)
forStmt = keyword "for" *> spaces *> () <*> keyword "in"

statement = between (try $ string "{%") (string "%}")
          $ between spaces spaces
          $ choice
          [ ifStmt 
          , forStmt
          
initialStatements :: Stream s m Char => ParsecT s u m [Statement]
initialStatements = many $ choice
        [ fmap IndentationStmt
                $ many1
                $ satisfy
                $ \x -> isSpace x && x `notElem` "\r\n"
        , fmap LineBreakStmt
                $ string "\n" <|> string "\r\n"
        , fmap CommentStmt
                $ (try $ string "{#") *> manyTill anyChar (try $ string "#}")
        , fmap ValueStmt
                $ between (try $ string "{{") (string "}}")
                $ between spaces spaces
                $ expression
        , fmap (PureStmt)
                $ many1
                $ notFollowedBy
                    (   (try $ string "{{")
                    <|> (try $ string "{%")
                    <|> (try $ string "{#")
                    <|> (string "\n")
                    <|> (string "\r\n"))
                *> anyChar
        ]

statement = 