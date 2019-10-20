module Parse where

import           Control.Arrow ((***))

newtype Parser a =
  Parser { runParser :: String -> [(a, String)] }

parse :: Parser a -> String -> a
parse p s =
  case runParser p s of
    [(result, [])] -> result
    [_]            -> error "Parser did not consume entire stream"
    _              -> error "Parser error"

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ fmap (f *** id) . p

instance Applicative Parser where
  pure x =
    Parser $ \s -> [(x, s)]
  Parser f <*> Parser p =
    Parser $ \s -> [ (f' a, s'') | (f', s') <- f s, (a, s'') <- p s' ]

instance Monad Parser where
  return = pure
  Parser p >>= f =
    Parser $ \s -> concatMap (\(a, s') -> runParser (f a) s') (p s)

alternative :: Parser a -> Parser a -> Parser a
alternative (Parser p) (Parser q) = Parser $ \s ->
  case p s of
    []  -> q s
    res -> res

failure :: Parser a
failure = Parser $ \s -> []

instance Alternative Parser where
  empty = failure
  (<|>) = alternative

combine :: Parser a -> Parser a -> Parser a
combine (Parser p) (Parser q) = Parser $ \s ->
  p s ++ q s

instance MonadPlus Parser where
  mzero = empty
  mplus = combine

item :: Parser Char
item = Parser $ \s ->
  case s of
    []     -> []
    (c:cs) -> [(c, cs)]
