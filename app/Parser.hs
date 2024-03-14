module Parser where

import Data.Functor.Identity (Identity)
import Text.Parsec hiding (Empty)
import Text.Parsec.Expr (Assoc (..), Operator (..), OperatorTable, buildExpressionParser)

type Ident = String

data Term
    = Empty
    | TSeq !Term !Term
    | TOr !Term !Term
    | TAnd !Term !Term
    | TPipe !Term !Term
    | TBang !Term
    | TSub !Term
    | TExternal !External
    | TBuiltin !Builtin
    deriving (Show, Eq, Ord)

data External = TCommand !Ident !Args
    deriving (Show, Eq, Ord)

data Builtin
    = TCd !Args
    | THelp !Args
    | TExit !Args
    deriving (Show, Eq, Ord)

data Args = ASub !Term | AList ![Ident]
    deriving (Show, Eq, Ord)

type Parser a = Parsec String () a
type Table a = OperatorTable String () Identity a

lexeme :: Parser a -> Parser a
lexeme p = spaces *> p <* spaces

forbidden :: String
forbidden = "(){}[]!;&| "

anyString :: Parser String
anyString = many1 $ noneOf forbidden

pArgs :: Parser Args
pArgs = choice [try pArgsSub, pArgsList]

pArgsSub :: Parser Args
pArgsSub = ASub <$> (string "$(" *> lexeme pTerm <* string ")")

pArgsList :: Parser Args
pArgsList = AList <$> (many1 (noneOf forbidden) `sepEndBy` spaces)

pCommand :: Parser Term
pCommand = choice [try pSubshell, try pBuiltin, TExternal <$> try pExternal]

pSubshell :: Parser Term
pSubshell = char '(' *> (TSub <$> pTerm) <* char ')'

pExternal :: Parser External
pExternal = TCommand <$> lexeme anyString <*> pArgs

pBuiltin :: Parser Term
pBuiltin = TBuiltin <$> choice [try pCd, try pExit, try pHelp]

pCd :: Parser Builtin
pCd = (TCd <$ lexeme (string "cd")) <*> pArgs

pExit :: Parser Builtin
pExit = (TExit <$ lexeme (string "exit")) <*> pArgs

pHelp :: Parser Builtin
pHelp = (THelp <$ lexeme (string "help")) <*> pArgs

pTerm :: Parser Term
pTerm = try expr <|> Empty <$ return ()

{- | TSeq !Term !Term
| TOr !Term !Term
| TAnd !Term !Term
| TPipe !Term !Term
| TBang !Term
-}
expr :: Parser Term
expr = buildExpressionParser table pCommand
  where
    table =
        [
            [ Prefix (TBang <$ string "! ")
            ]
        ,
            [ Infix (TPipe <$ char '|') AssocLeft
            , Infix (TSeq <$ char ';') AssocLeft
            , Infix (TOr <$ string "||") AssocLeft
            , Infix (TAnd <$ string "&&") AssocLeft
            ]
        ]

term :: String -> Either String Term
term s = case parse (pTerm <* eof) "" s of
    Left err -> Left $ "<parse failed>\n" <> show err
    Right r -> Right r
