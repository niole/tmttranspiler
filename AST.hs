data Expr a = Primitive a
  | Add (Expr a) (Expr a)
  | Subtract (Expr a) (Expr a)
  | Multiply (Expr a) (Expr a)
  | Divide (Expr a) (Expr a)
        deriving (Show)

-- 1 - 9 + 2 - 3 * 4 - 1 + 2 / 3

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

parseAddExpr :: Parser (Expr Int -> Expr Int)
parseAddExpr = (multiCombAdd <$> isAdd <*> parseNode <*> parseExpr) <|> (combAdd <$> isAdd <*> parseNode)
            where multiCombAdd _ n2 accer n1 = accer (Add n1 n2)
                  combAdd _ n2 n1 = Add n1 n2

parseSubtractExpr :: Parser (Expr Int -> Expr Int)
parseSubtractExpr = (multiCombSub <$> isSubtract <*> parseNode <*> parseExpr) <|> (combSub <$> isSubtract <*> parseNode)
            where multiCombSub _ n2 accer n1 = accer (Subtract n1 n2)
                  combSub _ n2 n1 = Subtract n1 n2

parseExpr :: Parser (Expr Int -> Expr Int)
parseExpr = parseAddExpr <|> parseSubtractExpr

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
parsePrimitive = skipSpaces *> (toPrim <$> (oneOrMore parseInt)) <* skipSpaces
                where toPrim n = Primitive (read n :: Int)

parseMultiply :: Parser (Expr Int)
parseMultiply = comb <$> parsePrimitive <*> isMultiply <*> parseNode
                where comb a _ b = Multiply a b

parseDivide :: Parser (Expr Int)
parseDivide = comb <$> parsePrimitive <*> isDivide <*> parseNode
              where comb a _ b = Divide a b

parseNode :: Parser (Expr Int)
parseNode = parseMultiply <|> parseDivide <|> parsePrimitive

startParse :: Parser (Expr Int)
startParse = (comb <$> parseNode <*> parseExpr) <|> parseNode
             where comb node accer = accer node

isDivide :: Parser Char
isDivide = satisfy $ \s -> s == '/'

isSubtract :: Parser Char
isSubtract = satisfy $ \s -> s == '-'

isMultiply :: Parser Char
isMultiply = satisfy $ \s -> s == '*'

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
