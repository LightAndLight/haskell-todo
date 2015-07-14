module Util.Date(
    Date(..)
    , parseDate
) where

import Control.Applicative
import Text.ParserCombinators.Parsec

data Date = Date { 
    year :: Int
    , month :: Int
    , day :: Int
} deriving (Eq, Ord)

instance Show Date where
    show (Date y m d) = show y ++ "-" ++ show m ++ "-" ++ show d

pYear :: Parser String
pYear = count 4 digit

pMonth :: Parser String
pMonth = count 2 digit

pDay :: Parser String
pDay = count 2 digit

sep :: Parser Char
sep = oneOf "./-"

international :: Parser (String,String,String)
international = (\y m d -> (y,m,d)) <$> pYear <*> (sep *> pMonth) <*> (sep *> pDay)

pDate :: Parser (String,String,String) 
pDate = spaces *> international <* spaces

parseDate :: String -> Either String Date
parseDate xs =
    case parse pDate "Date" xs of
        Right (y,m,d) -> Right $ Date (read y) (read m) (read d)
        Left _        -> Left "Invalid date format. Date must be in the format YYYY-MM-DD"

