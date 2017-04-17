{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Lish parser
module Lish.Parser
  ( parseCmd )
  where

import           Data.Fix
import qualified Data.Text        as Text
import           Protolude        hiding (for, many, optional, try, (<|>))
import           Text.Parsec
import           Text.Parsec.Text

import           Lish.Types

parseCmd :: Text -> Either ParseError Expr
parseCmd = parse parseExpr "S-Expr" . Text.strip . eatComment

eatComment :: Text -> Text
eatComment t =
  t
  & Text.lines
  & map (Text.takeWhile (/= ';'))
  & Text.intercalate "\n"

parseExpr :: Parser Expr
parseExpr = parseLambda
            <|> parseList
            <|> parseNumber
            <|> parseAtom
            <|> parseString

parseNumber :: Parser Expr
parseNumber = (Fix . Num . fromMaybe 0 . readMaybe) <$> many1 digit

parseAtom :: Parser Expr
parseAtom = do
  frst <- (noneOf " \t()[]\"")
  rest <- many (noneOf " \t()[]")
  case frst:rest of
    "true"  -> return . Fix $ Bool True
    "false" -> return . Fix $ Bool False
    x       -> return . Fix $ Atom (toS x)

parseString :: Parser Expr
parseString = (Fix . Str . toS) <$> between (char '"')
                                            (char '"')
                                            (many (noneOf "\""))

parseExprs :: Parser [Expr]
parseExprs = sepEndBy parseExpr spaces

parseLambda :: Parser Expr
parseLambda = Fix . Lambda <$> between (char '(') (char ')') parseExprs

parseList :: Parser Expr
parseList = Fix . List <$> between (char '[') (char ']') parseExprs
