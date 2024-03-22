{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Char (digitToInt, readLitChar)
import Data.List.NonEmpty (fromList)
import Data.Text (Text, pack)
import System.Posix (undl)
import Text.Parsec hiding (Empty)
import Text.Parsec.Expr (Assoc (..), Operator (..), buildExpressionParser)
import Types
import Prelude hiding (until)

lexeme :: Parser a -> Parser a
lexeme p = spaces *> p <* spaces

keySymbols :: String
keySymbols = " ();|&\"<>"

anyText :: Parser Text
anyText = pack <$> many1 (noneOf keySymbols)

anyString :: Parser String
anyString = many1 (noneOf keySymbols)

pArgsSub :: Parser Arg
pArgsSub = ASub <$> (string "$(" *> lexeme pTerm <* string ")")

pArgIdent :: Parser Arg
pArgIdent =
    AIdent . pack
        <$> choice
            [ try (char '"' *> fmap escape (many (noneOf "\"")) <* char '"')
            , anyString
            ]

escape :: String -> String
escape xs = case readLitChar xs of
    (c, rest) : _ -> c : escape rest
    [] -> []

pArg :: Parser Arg
pArg = choice [try pArgsSub, pArgIdent]

pArgs :: Parser [Arg]
pArgs = pArg `sepEndBy` spaces

pCommand :: Parser ([Arg] -> Term)
pCommand = choice [try pSubshell, try pBuiltin, try pExternal]

pCommandArgs :: Parser Term
pCommandArgs = pCommand <*> pArgs

pSubshell :: Parser ([Arg] -> Term)
pSubshell = char '(' *> (const . TSub <$> pTerm) <* char ')'

pExternal :: Parser ([Arg] -> Term)
pExternal = (TExternal .) <$> (External <$> lexeme anyText)

pBuiltin :: Parser ([Arg] -> Term)
pBuiltin = (TBuiltin .) <$> choice [try pCd, try pExit, try pPwd]

pCd :: Parser ([Arg] -> Builtin)
pCd = TCd <$ lexeme (string "cd")

pExit :: Parser ([Arg] -> Builtin)
pExit = TExit <$ lexeme (string "exit")

pPwd :: Parser ([Arg] -> Builtin)
pPwd = TPwd <$ lexeme (string "pwd")

pTerm :: Parser Term
pTerm = try pExpr <|> Empty <$ return ()

pRedirection :: Parser Term
pRedirection = do
    a <-
        try (pCommand <*> sepByUntil pArg spaces (digit *> pMode))
            <|> pCommand <*> sepByUntil pArg spaces pMode
    reds <- lexeme (fromList <$> many1 redirs)
    return (TRedirection a reds)
  where
    redirs :: Parser Redirection
    redirs = do
        (fd, mode) <-
            lexeme $
                (,)
                    <$> optionMaybe (digitToInt <$> digit)
                    <*> pMode
        Redirection fd mode . fromList <$> (anyString `sepEndBy1` spaces)

pMode :: Parser Mode
pMode =
    choice
        [ try (Append <$ string ">>")
        , try (ReadWrite <$ string "<>")
        , try (Write <$ char '>')
        , Read <$ char '<'
        ]

pExpr1 :: Parser Term
pExpr1 = choice [try pRedirection, try pCommandArgs]

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

sepByUntil :: Parser a -> Parser sep -> Parser end -> Parser [a]
sepByUntil p sep end = do
    ahead <- optionMaybe (lookAhead (try end))
    case ahead of
        Just _ -> return []
        Nothing -> (:) <$> p <* sep <*> sepByUntil p sep end

test :: Either ParseError Term
test = parse (pRedirection <* eof) "" "cat 9<> lol"
