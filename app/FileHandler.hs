module FileHandler where

readCsv = readFile "data.csv"
          >>= pure
          . groupById
          . map parseRow
          . filter (/= "")
          . tail
          . lines

notDuplicate :: Eq a => [a] -> [a]
notDuplicate [] = []
notDuplicate (x:xs)
  | x `elem` xs = notDuplicate xs
  | otherwise = x:(notDuplicate xs)

groupById :: [(Int, String, Float)]
          -> [[(String, Float)]]
groupById [] = []
groupById rows = groups . notDuplicate . map (\(i, _, _) -> i) $ rows
  where groups [] = []
        groups (i:ids) = [ map (\(_, k, v) -> (k, v))
                           . filter (\(i', _, _) -> i' == i)
                           $ rows
                         ] ++ groups ids

parseRow :: String
         -> (Int, String, Float)
parseRow s = let chartId = read (takeWhile (/= ',') s) :: Int
                 fromId = tail . dropWhile (/= ',') $ s
                 key = takeWhile (/= ',') fromId
                 val = read (tail . dropWhile (/= ',') $ fromId) :: Float
             in (chartId, key, val)
