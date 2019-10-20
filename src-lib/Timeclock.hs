module Timeclock where

import qualified Data.Time.Calendar as Calendar
import           Time               (Time, mkTime)

data DateTime =
  DateTime
  { date :: Calendar.Day
  , time :: Time
  }

mkDateTime
  :: Integer
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Maybe DateTime
mkDateTime y m d h mi s = do
  time <- mkTime h mi s
  day <- Calendar.fromGregorianValid y m d
  return $ DateTime day time

data Entry
  = ClockIn DateTime String
  | ClockOut DateTime (Maybe String)
