module ParseHelpers (parseInt, skipSpaces, zeroOrMore, oneOrMore, isInt, isSpace, satisfy) where

import Parse (Parser(..), (<|>), (<$>), (<*>))

parseInt :: Parser Char
parseInt = satisfy isInt

skipSpaces :: Parser String
skipSpaces = zeroOrMore $ satisfy isSpace

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

isInt :: Char -> Bool
isInt s = case s of
  '0' -> True
  '1' ->True
  '2' ->True
  '3' ->True
  '4' ->True
  '5' ->True
  '6' ->True
  '7' ->True
  '8' ->True
  '9' ->True
  _ -> False

isSpace :: Char -> Bool
isSpace s = s == ' '

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
        where f [] = Nothing
              f (x:xs)
                | p x = Just (x, xs)
                | otherwise = Nothing
