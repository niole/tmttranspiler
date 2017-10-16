import Parse
import ParseHelpers

data Expr a = Primitive a
  | Add (Expr a) (Expr a)
  | Subtract (Expr a) (Expr a)
  | Multiply (Expr a) (Expr a)
  | Divide (Expr a) (Expr a)
        deriving (Show)

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

toAST :: String -> Maybe (Expr Int)
toAST s = getExpression <$> parse startParse s
          where getExpression (parsed, _) =  parsed

fromAST :: Expr Int -> String
fromAST (Primitive n) = show n
fromAST (Add a b) = fromAST a ++ "+" ++ fromAST b
fromAST (Subtract a b) = fromAST a ++ "-" ++ fromAST b
fromAST (Multiply a b) = fromAST a ++ "*" ++ fromAST b
fromAST (Divide a b) = fromAST a ++ "/" ++ fromAST b
