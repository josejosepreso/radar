module Rendering where

import Graphics.Gloss
import Logic

width, height, offset :: Int
width = 600
height = 600
offset = 20

scoreS, labelS :: Float
scoreS = 0.05
labelS = 0.1

window :: Display
window = InWindow "radar" (width, height) (offset, offset)

background :: Color
background = white

chartColor :: [(Picture -> Picture)]
chartColor = map (color . dark) [ green, orange, yellow, blue, red ]

equal :: Eq a => [[a]] -> Bool
equal [] = False
equal xs = let pivot = head xs
           in areEq pivot xs
  where areEq _ [] = True
        areEq p (y:ys)
          | p == y = areEq p ys
          | otherwise = False

equalSize :: [[a]] -> Bool
equalSize [] = False
equalSize xs = let size = length . head $ xs
               in sameSize size xs
  where sameSize _ [] = True
        sameSize n (y:ys)
          | length y == n = sameSize n ys
          | otherwise = False

net :: [(Point, Point)]
    -> [String]
    -> [Picture]
net _ [] = []
net [] _ = []
net xs labels = let values = map (\n -> 2 * (n / 10)) [1 .. 5]
                in [ Pictures $ innerAxis xs i
                   | i <- values
                   ]
                   ++
                   [ Pictures [ Line [ (0, 0), p ]
                              , Translate (fst p) (10 + snd p)
                                $ Scale labelS labelS
                                $ Text
                                $ label
                              ]
                   | (label, p) <- zip labels (map fst xs)
                   ]
  where innerAxis [] _ = []
        innerAxis ((p1, p2):ys) a
          = let p1_a = scaleV a p1
                p2_a = scaleV a p2
                x = fst p1_a
                y = snd p1_a
            in [ Pictures [ Line [ p1_a, p2_a ]
                          , Translate x y
                            $ Scale scoreS scoreS
                            $ Text
                            $ show (round $ a * 100)
                          ]
               ]
               ++ innerAxis ys a

picture :: [[(Point, Point)]]
        -> [Picture]
picture [] = []
picture (x:xs) = [ Pictures [ Pictures $ pointsLine x
                            , Pictures $ point x
                            ]
                 ]
                 ++ picture xs
  where
    pointsLine [] = []
    pointsLine ((p1, p2):ys) = [ Line [ p1, p2 ]
                               ]
                               ++ pointsLine ys
    point [] = []
    point ((p1, _):ys) = [ Translate (fst p1) (snd p1)
                           $ circleSolid 5
                         ]
                         ++ point ys                        

-- TODO: get intersections
chart [] = pure ()
chart pairs
  | or [ not . equalSize $ pairs
       , not . equal $ labelsById
       ] = putStrLn "Bad data file format."
  | otherwise = display window background
                . Pictures
                $ polygons ++ radars ++ netPict
  where
    labelsById = map (map fst) pairs
    values = map (map (\(_, value) -> value / 100)) pairs
    labels = map fst
             . head
             $ pairs
    maxCoords = getNetCoordinates
                $ length labels
    netPict = (net . makePaths $ maxCoords) labels
    paths = map makePaths
            . map (map (\(p, a) -> scaleV a p))
            . map (zip maxCoords)
            $ values
    radars = picture paths
    polygons = map getPolygon
               $ zip (map (map fst) paths) [0 .. pred . length $ pairs]
    getPolygon (ps, i) = (chartColor !! (i `mod` length chartColor))
                         . Polygon
                         $ ps
