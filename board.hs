-- Mauricio Roberto Hidalgo & Mark Nunez 
-- Programming Assignment 3
-- Haskell
-- CS3360 TR 10:30 - 11:50 
-- 12/06/18

module Board(mkBoard) where

import Data.List
import Data.Array

-- Generates a default mxn board containing 0's
mkBoard :: Int -> Int -> [[Int]]
mk Board m n = replicate m (replicate n 0) 

