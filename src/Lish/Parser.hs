{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Lish parser
module Lish.Parser
  (parseCmd)
  where

import           Prelude     (String)
import           Protolude   hiding (for, many, (<|>))
import           Text.Parsec

import           Lish.Types

parseCmd :: String -> Either ParseError SExp
parseCmd = parse parseExpr "S-Expr"

parseExpr :: Parsec String () SExp
parseExpr = parseLambda
            <|> parseList
            <|> parseAtom
            <|> parseString

parseAtom :: Parsec String () SExp
parseAtom = Atom <$> do frst <- (noneOf " \t()[]\"")
                        rest <- many (noneOf " \t()[]")
                        return $ toS (frst:rest)

parseString :: Parsec String () SExp
parseString = (Str . toS) <$> between (char '"')
                              (char '"')
                              (many (noneOf "\""))

parseSExps :: Parsec String () [SExp]
parseSExps = sepEndBy parseExpr spaces

parseLambda :: Parsec String () SExp
parseLambda = Lambda <$> between (char '(') (char ')') parseSExps

parseList :: Parsec String () SExp
parseList = List <$> between (char '[') (char ']') parseSExps
