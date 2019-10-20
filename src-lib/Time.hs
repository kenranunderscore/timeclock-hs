module Time
  ( Time
  , mkTime
  , Hour
  , Minute
  , Second
  )
where

-- potential FIXME: use DataKinds for the following types

newtype Hour =
  Hour { hourValue :: Int }

mkHour :: Int -> Maybe Hour
mkHour h
  | h >= 0 && h < 24 = Just $ Hour h
  | otherwise = Nothing

newtype Minute =
  Minute { minuteValue :: Int }

mkMinute :: Int -> Maybe Minute
mkMinute m
  | m >= 0 && m < 60 = Just $ Minute m
  | otherwise = Nothing

newtype Second =
  Second { secondValue :: Int }

mkSecond :: Int -> Maybe Second
mkSecond s
  | s >= 0 && s < 60 = Just $ Second s
  | otherwise = Nothing

data Time =
  Time
  { hour   :: Hour
  , minute :: Minute
  , second :: Second
  }

mkTime :: Int -> Int -> Int -> Maybe Time
mkTime h m s = do
  h' <- mkHour h
  m' <- mkMinute m
  s' <- mkSecond s
  return $ Time h' m' s'
