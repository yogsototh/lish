{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Lish parser
module Lish.Parser
  ( parseCmd )
  where

import           Protolude        hiding (for, many, optional, try, (<|>))
import           Text.Parsec
import           Text.Parsec.Text

import           Lish.Types

parseCmd :: Text -> Either ParseError SExp
parseCmd = parse parseExpr "S-Expr"

parseExpr :: Parser SExp
parseExpr = parseLambda
            <|> parseList
            <|> parseNumber
            <|> parseAtom
            <|> parseString

parseNumber :: Parser SExp
parseNumber = (Num . fromMaybe 0 . readMaybe) <$> many1 digit

parseAtom :: Parser SExp
parseAtom = do
  frst <- (noneOf " \t()[]\"")
  rest <- many (noneOf " \t()[]")
  case frst:rest of
    "true"  -> return (Bool True)
    "false" -> return (Bool False)
    x       -> return (Atom (toS x))

parseString :: Parser SExp
parseString = (Str . toS) <$> between (char '"')
                              (char '"')
                              (many (noneOf "\""))

parseSExps :: Parser [SExp]
parseSExps = sepEndBy parseExpr spaces

parseLambda :: Parser SExp
parseLambda = Lambda <$> between (char '(') (char ')') parseSExps

parseList :: Parser SExp
parseList = List <$> between (char '[') (char ']') parseSExps
