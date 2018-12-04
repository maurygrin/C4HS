-- Mauricio Roberto Hidalgo & Mark Nunez 
-- Programming Assignment 3
-- Haskell
-- CS3360 TR 10:30 - 11:50 
-- 12/06/18

module Board(mkBoard, mkPlayer, mkOpponent, changeTurn) where

import Data.List
import Data.Array

-- Generates a default mxn board containing 0's
mkBoard :: Int -> Int -> [[Int]]
mkBoard m n = replicate m (replicate n 0) 

-- making players
mkPlayer :: Int
mkPlayer = 1

mkOpponent :: Int
mkOpponent = 2

--taking turns
changeTurn :: Int -> Int
changeTurn turn
     |turn == 1 = mkOpponent
     |otherwise = mkPlayer