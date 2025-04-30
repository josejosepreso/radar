module Main where

import FileHandler
import Rendering

main = readCsv >>= chart
