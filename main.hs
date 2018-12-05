-- Mauricio Roberto Hidalgo & Mark Hiram Nunez
-- Programming Assignment 3
-- Haskell
-- CS3360 TR 10:30 - 11:50
-- 12/06/18

import Data.List
import System.IO
import Board

-- Main of the proyect that starts the game
main = do
     let bd = mkBoard 7 6
     let p = mkPlayer
 
-- Return a char representation of a player p.
playerToChar :: Int -> String
playerToChar p 
     | (p == 1) = "O "
     | (p == 2) = "X "
     | otherwise = ". "