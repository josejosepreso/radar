module Rendering where

import Graphics.Gloss
import Logic

width, height, offset :: Int
width = 600
height = 600
offset = 20

scoreS, labelS, pointSize :: Float
scoreS = 0.05
labelS = 0.1
pointSize = 4.5

window :: Display
window = InWindow "radar" (width, height) (offset, offset)

background :: Color
background = white

chartColor :: Int -> (Picture -> Picture)
chartColor i = let colors = map (color . dark) [ rose, orange, yellow, cyan, red ]
               in colors !! (i `mod` length colors)

innerAxis :: Path -> Float -> [Picture]
innerAxis [] _ = []
innerAxis xs a = [ Line scale ]
                 ++                         
                 [ Pictures [ Translate x y
                              $ Scale scoreS scoreS
                              $ Text
                              $ level
                            | (x, y) <- scale
                            ]
                 ]
  where scale = map (scaleV a) xs
        level = show . round $ a * 100

net :: Path -> [String] -> [Picture]
net _ [] = []
net [] _ = []
net xs labels = let levels = map (\n -> 2 * (n / 10)) [1 .. 5]
                    path = xs ++ [head xs]
                in [ Pictures $ innerAxis path i | i <- levels ]
                   ++
                   [ Pictures [ Line [ (0, 0), p ]
                              , Translate (fst p) (10 + snd p)
                                $ Scale labelS labelS
                                $ Text
                                $ label
                              ]
                   | (label, p) <- zip labels path
                   ]
                   
picture :: Path -> [Picture]
picture [] = []
picture xs = let path = xs ++ [head xs]
             in (Line path):[ Translate x y $ circleSolid pointSize | (x, y) <- path ]

-- TODO: get intersections
chart :: [[(String, Float)]] -> IO ()
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
    labels = map fst . head $ pairs
    
    maxCoords = getNetCoordinates $ length labels
    
    netPict = net maxCoords labels
    
    paths = map (map (\(p, a) -> scaleV a p))
            . map (zip maxCoords)
            $ values
    radars = flatten . map picture $ paths
    
    polygons = map getPolygon $ zip paths [0 .. pred . length $ pairs]
    getPolygon (path, i) = chartColor i . Polygon $ path
