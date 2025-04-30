module Logic ( scaleV
             , getNetCoordinates
             , makePaths
             ) where

type Point = (Float, Float)

d, r :: Float
d = 500
r = d / 2

scaleV :: Float
       -> Point
       -> Point
scaleV n (a, b) = (n * a, n * b)

getNetCoordinates :: Int
                  -> [Point]
getNetCoordinates n = [ coord j
                      | j <- [1 .. n]
                      ]
  where
    coord i = let idx = (fromIntegral i) :: Float
                  size = (fromIntegral n) :: Float
                  x = r * cos (idx * 2 * pi / size)
                  y = r * sin (idx * 2 * pi / size)
              in (x, y)

makePaths :: [Point]
          -> [(Point, Point)]
makePaths [] = []
makePaths xs = findPaths (head xs) xs
  where
    findPaths first [p]        = [(p, first)]
    findPaths first (p1:p2:ys) = [(p1, p2)]
                                 ++ findPaths first (p2:ys)
