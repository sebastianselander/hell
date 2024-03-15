module Parser where

import Text.Parsec hiding (Empty)
import Text.Parsec.Expr (Assoc (..), Operator (..), buildExpressionParser)
import Types

lexeme :: Parser a -> Parser a
lexeme p = spaces *> p <* spaces

forbidden :: String
forbidden = "(){}[]!;&| "

anyString :: Parser String
anyString = many1 $ noneOf forbidden

pArgsSub :: Parser Arg
pArgsSub = ASub <$> (string "$(" *> lexeme pTerm <* string ")")

pArgIdent :: Parser Arg
pArgIdent = AIdent <$> many1 (noneOf forbidden)

pArgs :: Parser [Arg]
pArgs = choice [try pArgsSub, try pArgIdent] `sepEndBy` spaces

pCommand :: Parser Term
pCommand = choice [try pSubshell, try pBuiltin, TExternal <$> try pExternal]

pSubshell :: Parser Term
pSubshell = char '(' *> (TSub <$> pTerm) <* char ')'

pExternal :: Parser External
pExternal = TCommand <$> lexeme anyString <*> pArgs

pBuiltin :: Parser Term
pBuiltin = TBuiltin <$> choice [try pCd, try pExit, try pPwd]

pCd :: Parser Builtin
pCd = (TCd <$ lexeme (string "cd")) <*> pArgs

pExit :: Parser Builtin
pExit = (TExit <$ lexeme (string "exit")) <*> pArgs

pTerm :: Parser Term
pTerm = try expr <|> Empty <$ return ()

pPwd :: Parser Builtin
pPwd = (TPwd <$ lexeme (string "pwd")) <*> pArgs

expr :: Parser Term
expr = buildExpressionParser table (lexeme pCommand)
  where
    table =
        [
            [ Prefix (TBang <$ char '!' <* space)
            ]
        ,
            [ Infix (TOr <$ string "||") AssocLeft
            , Infix (TAnd <$ string "&&") AssocLeft
            , Infix (TPipe <$ char '|') AssocLeft
            , Infix (TSeq <$ char ';') AssocLeft
            ]
        ]

term :: String -> Either String Term
term s = case parse (pTerm <* eof) "" s of
    Left err -> Left $ "<parse failed>\n" <> show err
    Right r -> Right r
