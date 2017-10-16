module Parse (Parser(..), (<|>), (<$>), (<*>)) where

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
