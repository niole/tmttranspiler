data Expr a = Primitive a
  | Add (Expr a) (Expr a)
  | Subtract (Expr a) (Expr a)
  | Multiply (Expr a) (Expr a)
  | Divide (Expr a) (Expr a)
        deriving (Show)

newtype Parser a = Parser { parse :: String -> Maybe(a, String) }

instance Functor Parser where
  fmap f p = Parser mapped
                where mapped xs =  (\(a, b) -> (f a, b)) <$> (parse p xs)

instance Applicative Parser where
  pure x = Parser $ \s -> Just(x, s)
  fp <*> p = Parser apped
                where apped xs = case parse fp xs of
                                 Nothing -> Nothing
                                 Just(f, rest) -> parse (f <$> p) rest

class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (<|>) pa pb = Parser fa
                where fa xs = case parse pa xs of
                                Nothing -> parse pb xs
                                x -> x

isSpace :: Char -> Bool
isSpace s = s == ' '

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
        where f [] = Nothing
              f (x:xs)
                | p x = Just (x, xs)
                | otherwise = Nothing

parseInt :: Parser Char
parseInt = satisfy isInt

parsePrimitive :: Parser (Expr Int)
parsePrimitive = toPrim <$> (oneOrMore parseInt)
                where toPrim n = Primitive (read n :: Int)

parseAdd :: Parser (Expr Int)
parseAdd = comb <$> (parsePrimitive <* skipSpaces) <*> isAdd <*> (skipSpaces *> parseAdd <|> parsePrimitive)
                where comb p1 _ p2 = Add p1 p2

isAdd :: Parser Char
isAdd = satisfy $ \s -> s == '+'

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
