module Parse
  ( Parser(..)
  , parse
  , failure
  , item
  , satisfy
  , char
  , space
  , untilBefore
  , digit
  , natural
  , string
  , count
  , utcTime
  )
where

import           Control.Applicative
import           Control.Arrow       ((***))
import           Control.Monad
import           Data.Char
import           Data.Time.Clock     (UTCTime)
import           Data.Time.Format    as Format

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

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  c <- item
  if p c then
    return c
  else
    failure

char :: Char -> Parser Char
char c =
  satisfy (c ==)

space :: Parser Char
space =
  char ' '

untilBefore :: (Char -> Bool) -> Parser String
untilBefore p =
  many (satisfy (not . p))

digit :: Parser Char
digit =
  satisfy isDigit

natural :: Parser Integer
natural =
  read <$> some digit

string :: String -> Parser String
string =
  mapM char

count :: Int -> Parser a -> Parser [a]
count n p =
  sequenceA $ replicate n p

instance Semigroup a => Semigroup (Parser a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (Parser a) where
  mempty = pure mempty
  mappend = (<>)

utcTime :: Parser UTCTime
utcTime =
  let twoDigits = Parse.count 2 Parse.digit in
    (Parse.count 4 Parse.digit)
    <> Parse.string "/"
    <> twoDigits
    <> Parse.string "/"
    <> twoDigits
    <> Parse.string " "
    <> twoDigits
    <> Parse.string ":"
    <> twoDigits
    <> Parse.string ":"
    <> twoDigits
    >>= Format.parseTimeM False Format.defaultTimeLocale "%0Y/%m/%d %H:%M:%S"
