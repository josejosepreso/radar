module Main where

import Graphics.Gloss

width, height, offset :: Int
width = 600
height = 600
offset = 20

window :: Display
window = InWindow "radar" (width, height) (offset, offset)

background :: Color
background = white

d = 500

readCsv = readFile "data.csv" >>= pure . map parsePair . lines

parsePair :: String -> (String, Float)
parsePair s = let key = takeWhile (/= ',') s
                  val = read (tail . dropWhile (/= ',') $ s) :: Float
              in (key, val)

axis :: Float -> Int -> [Picture]
axis 0 _ = []
axis r n = [ Pictures [ Line [ p1, p2 ]
                      , Line [ (0, 0), p1 ]
                      ]
           | (p1, p2) <- paths
           ] ++ axis (r - d / 10) n
  where
    paths = [ ( coord i
              , coord (succ i)
              )
            | i <- [1 .. size]
            ]
    coord i = ( r * sin (i * 2 * pi / size)
              , r * cos (i * 2 * pi / size)
              )
    size = (fromIntegral n) :: Float
                
chart pairs = display window background (Pictures $ axis (d / 2) (length pairs))

main = readCsv >>= chart
