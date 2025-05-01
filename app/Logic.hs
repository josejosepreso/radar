module Logic ( scaleV
             , getNetCoordinates
             , flatten
             , equal
             , equalSize
             ) where

type Point = (Float, Float)

d, r :: Float
d = 500
r = d / 2

scaleV :: Float -> Point -> Point
scaleV n (a, b) = (n * a, n * b)

getNetCoordinates :: Int -> [Point]
getNetCoordinates n = [ coord j
                      | j <- [1 .. n]
                      ]
  where
    coord i = let idx = (fromIntegral i) :: Float
                  size = (fromIntegral n) :: Float
                  x = r * cos (idx * 2 * pi / size)
                  y = r * sin (idx * 2 * pi / size)
              in (x, y)

flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = [] ++ x ++ flatten xs

equal :: Eq a => [[a]] -> Bool
equal [] = False
equal xs = let pivot = head xs
           in areEq pivot xs
  where areEq _ [] = True
        areEq p (y:ys) = p == y && areEq p ys

equalSize :: [[a]] -> Bool
equalSize [] = False
equalSize xs = let size = length . head $ xs
               in sameSize size xs
  where sameSize _ [] = True
        sameSize n (y:ys) = length y == n && sameSize n ys
