module Lib where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

data LispValue = Atom String
               | List [LispValue]
               | DottedList [LispValue] LispValue
               | Number Integer
               | String String
               | Bool Bool
               deriving Show

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

nonQuotedChar :: Parser Char
nonQuotedChar = noneOf "\""

parseString :: Parser LispValue
parseString = do
                char '"'
                x <- string "\"" <|> many nonQuotedChar
                char '"'
                return $ String x


parseAtom :: Parser LispValue
parseAtom = do
  first <- letter <|> symbol
  rest <- many $ letter <|> digit <|> symbol
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    otherwise -> Atom atom

parseNumber :: Parser LispValue
parseNumber = many1 digit >>= (\digits -> return $  Number . read $ digits)

parseExpr = parseAtom
        <|> parseString
        <|> parseNumber


readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left error -> "No match: " ++ show error
                   Right value -> "Found value" ++ show value
