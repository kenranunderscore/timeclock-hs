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
  (Parser f) <*> (Parser p2) =
    Parser $ \s -> [  ]
