module Main where

import Graphics.Gloss

width, height, offset :: Int
width = 600
height = 600
offset = 20

d, r, scoreS, labelS :: Float
d = 500
r = d / 2
scoreS = 0.05
labelS = 0.1

window :: Display
window = InWindow "radar" (width, height) (offset, offset)

background :: Color
background = white

readCsv = readFile "data.csv"
          >>= pure
          . map parsePair
          . lines

parsePair :: String
          -> (String, Float)
parsePair s = let key = takeWhile (/= ',') s
                  val = read (tail . dropWhile (/= ',') $ s) :: Float
              in (key, val)

scaleV :: Float
       -> Point
       -> Point
scaleV n (a, b) = (n * a, n * b)

points :: [(String, Float)]
       -> [(Point, Point)]
points pairs = [ let c = coord i
                 in ( c
                    , scaleV value c
                    )
               | (i, value) <- zip [1 .. n] values
               ]
  where
    n = length pairs
    values = map (\(_, n) -> n / 100) pairs
    coord i = let idx = (fromIntegral i) :: Float
                  size = (fromIntegral n) :: Float
                  x = r * cos (idx * 2 * pi / size)
                  y = r * sin (idx * 2 * pi / size)
              in (x, y)

makePaths :: [(Point, Point)]
          -> [((Point, Point), (Point, Point))]
makePaths [] = []
makePaths xs = let first = head xs
               in findPaths first xs
  where
    findPaths first [(x, y)] = [((x, fst first), (y, snd first))]
    findPaths first ((a, b):(c, d):xs) = [((a, c), (b, d))]
                                         ++ findPaths first ((c, d):xs)

picture [] _ = []
picture ((pp1, pp2):xs) (l:ls) = [ Pictures [ Line [ fst pp1, snd pp1 ]
                                            , Translate (fst . fst $ pp1) (snd . fst $ pp1)
                                              $ Scale scoreS scoreS
                                              $ Text "100"
                                            , Line [ fst pp2, snd pp2 ]
                                            , Translate (fst . fst $ pp2) (snd . fst $ pp2)
                                              $ circleSolid 5
                                            , Translate ((+) 5 . fst . fst $ pp2) (snd . fst $ pp2)
                                              $ Scale labelS labelS
                                              $ Text l
                                            , Line [ (0, 0), fst pp1 ]
                                            ]
                                 ]
                                 ++ picture xs ls
                                 ++ axis (fst pp1) (snd pp1) (1 - 0.2)
  where axis p1 p2 s
          | s <= 0 = []
          | otherwise = let p' = scaleV s p1
                            p'' = scaleV s p2
                            x = fst p'
                            y = snd p'
                        in [ Pictures [ Line [ p', p'' ]
                                      , Translate x y $ Scale scoreS scoreS $ Text $ show (round $ s * 100)
                                      ]
                           ] ++ axis p1 p2 (s - 0.2)

chart [] = pure ()
chart pairs = display window background
              . Pictures
              $ [ polygon ]
              ++ radar
  where
    paths = makePaths . points $ pairs
    polygon = (color $ dark red)
              $ Polygon
              . map (snd . snd)
              $ paths
    radar = picture paths
            $ map fst pairs

main = readCsv >>= chart
