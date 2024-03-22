module Parser where

import Data.List.NonEmpty (fromList)
import Data.Text (Text, pack)
import Text.Parsec hiding (Empty)
import Text.Parsec.Expr (Assoc (..), Operator (..), buildExpressionParser)
import Types

lexeme :: Parser a -> Parser a
lexeme p = spaces *> p <* spaces

forbidden :: String
forbidden = "(){}[]!;&|<> "

-- TODO: Make custom many, many1 parsers for text
anyText :: Parser Text
anyText = pack <$> many1 (noneOf forbidden)

anyString :: Parser String
anyString = many1 (noneOf forbidden)

pArgsSub :: Parser Arg
pArgsSub = ASub <$> (string "$(" *> lexeme pTerm <* string ")")

pArgIdent :: Parser Arg
pArgIdent = AIdent . pack <$> many1 (noneOf forbidden)

pArgs :: Parser [Arg]
pArgs = choice [try pArgsSub, try pArgIdent] `sepEndBy` spaces

pCommand :: Parser Term
pCommand = choice [try pSubshell, try pBuiltin, try pExternal]

pSubshell :: Parser Term
pSubshell = char '(' *> (TSub <$> pTerm) <* char ')'

pExternal :: Parser Term
pExternal = TExternal <$> (External <$> lexeme anyText <*> pArgs)

pBuiltin :: Parser Term
pBuiltin = TBuiltin <$> choice [try pCd, try pExit, try pPwd]

pCd :: Parser Builtin
pCd = (TCd <$ lexeme (string "cd")) <*> pArgs

pExit :: Parser Builtin
pExit = (TExit <$ lexeme (string "exit")) <*> pArgs

pTerm :: Parser Term
pTerm = try pExpr <|> Empty <$ return ()

pPwd :: Parser Builtin
pPwd = (TPwd <$ lexeme (string "pwd")) <*> pArgs

pRedirection :: Parser Term
pRedirection = do
    command <- pCommand
    TRedirection command . fromList <$> many1 redirs
  where
    redirs :: Parser Redirection
    redirs = do
        mode <-
            lexeme $
                choice
                    [ try (Append <$ string ">>")
                    , try (ReadWrite <$ string "<>")
                    , try (Write <$ char '>')
                    , try (Read <$ char '<')
                    ]
        Redirection mode . fromList <$> (anyString `sepEndBy1` spaces)

pExpr1 :: Parser Term
pExpr1 = choice [try pRedirection, try pCommand]

pExpr :: Parser Term
pExpr = buildExpressionParser table (lexeme pExpr1)
  where
    table =
        [
            [ Infix (TPipe <$ try (char '|')) AssocLeft
            ]
        ,
            [ Prefix (TBang <$ char '!' <* space)
            ]
        ,
            [ Infix (TOr <$ try (string "||")) AssocLeft
            , Infix (TAnd <$ try (string "&&")) AssocLeft
            , Infix (TSeq <$ try (char ';')) AssocLeft
            ]
        ]

term :: Text -> Either String Term
term s = case parse (pTerm <* eof) "" s of
    Left err -> Left $ "<parse failed>\n" <> show err
    Right r -> Right r
