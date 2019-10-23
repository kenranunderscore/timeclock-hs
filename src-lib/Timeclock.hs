module Timeclock where

import           Control.Applicative (many, optional, some, (<|>))
import           Data.Time.Clock     (UTCTime)
import           Parse               (Parser)
import qualified Parse

data Entry
  = ClockIn UTCTime String
  | ClockOut UTCTime (Maybe String)
  deriving Show

type Timelog =
  [Entry]

timelog :: Parser Timelog
timelog =
  many entry

entry :: Parser Entry
entry =
  clockIn <|> clockOut

clockOut :: Parser Entry
clockOut = do
  _ <- Parse.char 'o'
  _ <- Parse.space
  time <- Parse.utcTime
  desc <- optional (Parse.string " " <> Parse.untilBefore (== '\n')) <* Parse.char '\n'
  return $ ClockOut time desc

clockIn :: Parser Entry
clockIn = do
  _ <- Parse.char 'i'
  _ <- Parse.space
  time <- Parse.utcTime
  _ <- Parse.space
  desc <- Parse.untilBefore (== '\n') <* Parse.char '\n'
  return $ ClockIn time desc

readTimelog :: FilePath -> IO Timelog
readTimelog path =
  Parse.parse timelog <$> readFile path
