{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Lish parser
module Lish.Parser
  (parseCmd)
  where

import           Protolude   hiding (for, many, (<|>), optional)
import           Text.Parsec
import           Text.Parsec.Text

import           Lish.Types

parseCmd :: Text -> Either ParseError SExp
parseCmd = parse parseExpr "S-Expr"

parseExpr :: Parser SExp
parseExpr = parseLambda
            <|> parseList
            <|> parseBool
            <|> parseNumber
            <|> parseAtom
            <|> parseString

parseNumber :: Parser SExp
parseNumber = (Num . fromMaybe 0 . readMaybe) <$> many1 digit

parseBool :: Parser SExp
parseBool = Bool <$> ((string "true" >> return True)
                      <|> (string "false" >> return False))

parseAtom :: Parser SExp
parseAtom = Atom <$> do frst <- (noneOf " \t()[]\"")
                        rest <- many (noneOf " \t()[]")
                        return $ toS (frst:rest)

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
