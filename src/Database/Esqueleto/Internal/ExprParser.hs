{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | This is an internal module. This module may have breaking changes without
-- a corresponding major version bump. If you use this module, please open an
-- issue with your use-case so we can safely support it.
module Database.Esqueleto.Internal.ExprParser where

import           Prelude              hiding (takeWhile)

import           Control.Applicative  ((<|>))
import           Control.Monad        (void)
import           Data.Attoparsec.Text
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Database.Persist.Sql

-- | A type representing the access of a table value. In Esqueleto, we get
-- a guarantee that the access will look something like:
--
-- @
-- escape-char [character] escape-char . escape-char [character] escape-char
--             ^^^^^^^^^^^                           ^^^^^^^^^^^
--             table name                            column name
-- @
data TableAccess = TableAccess
  { tableAccessTable  :: Text
  , tableAccessColumn :: Text
  }
  deriving (Eq, Ord, Show)

-- | Parse a @SqlExpr (Value Bool)@'s textual representation into a list of
-- 'TableAccess'
parseOnExpr :: SqlBackend -> Text -> Either String (Set TableAccess)
parseOnExpr sqlBackend text = do
  c <- mkEscapeChar sqlBackend
  parseOnly (onExpr c) text

-- | This function uses the 'connEscapeName' function in the 'SqlBackend' with an
-- empty identifier to pull out an escape character. This implementation works
-- with postgresql, mysql, and sqlite backends.
mkEscapeChar :: SqlBackend -> Either String Char
mkEscapeChar sqlBackend =
  case Text.uncons (connEscapeName sqlBackend (DBName "")) of
    Nothing ->
      Left "Failed to get an escape character from the SQL backend."
    Just (c, _) ->
      Right c

type ExprParser a = Char -> Parser a

onExpr :: ExprParser (Set TableAccess)
onExpr e = Set.fromList <$> many' tableAccesses
  where
   tableAccesses = do
     skipToEscape e <?> "Skipping to an escape char"
     parseTableAccess e <?> "Parsing a table access"

skipToEscape :: ExprParser ()
skipToEscape escapeChar = void (takeWhile (/= escapeChar))

parseEscapedIdentifier :: ExprParser [Char]
parseEscapedIdentifier escapeChar = do
  char escapeChar
  str <- parseEscapedChars escapeChar
  char escapeChar
  pure str

parseTableAccess :: ExprParser TableAccess
parseTableAccess ec = do
  tableAccessTable <- Text.pack <$> parseEscapedIdentifier ec
  _ <- char '.'
  tableAccessColumn <- Text.pack <$> parseEscapedIdentifier ec
  pure TableAccess {..}

parseEscapedChars :: ExprParser [Char]
parseEscapedChars escapeChar = go
  where
    twoEscapes = char escapeChar *> char escapeChar
    go = many' (notChar escapeChar <|> twoEscapes)
