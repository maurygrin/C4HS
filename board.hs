-- Mauricio Roberto Hidalgo & Mark Nunez 
-- Programming Assignment 3
-- Haskell
-- CS3360 TR 10:30 - 11:50 
-- 12/06/18

module Board(mkBoard, mkPlayer, mkOpponent, changeTurn) where

import Data.List
import Data.Array

--1. (8 points) Creating a board and players.----------------------------------------

-- Generates a default mxn board containing 0's
mkBoard :: Int -> Int -> [[Int]]
mkBoard m n = replicate m (replicate n 0) 

-- making players
mkPlayer :: Int
mkPlayer = 1

mkOpponent :: Int
mkOpponent = 2


--2. (20 points) Checking a board and dropping a disc---------------------------------

dropInSlot :: [[Int]] -> Int -> Int -> [[Int]]
dropInSlot bd i p = makeNewCol bd i newCol
    where curCol = getCol bd i;
          emptyPos = getNTP curCol;
          newCol = insertToken emptyPos p currCol

--get the column for the index provided
getCol :: [[Int]] -> Int -> [Int]   
getColumn bd i = bd !! i

--get Next Token Placement
getNTP col = last (findIndices(==0) col)

--puts player's token at this position
insertToken i token (x:xs)
    | i == 0 = token:xs
    | otherwise = x:insertToken (i-1) token xs

--makes a new column with the updated values
makeNewCol :: [[Int]] -> Int -> [Int] -> [[Int]]
makeNewCol bd i newCol = front ++ [newCol] ++ end
    where front = take (i) bd;
          end = drop (i + 1) bd

-- checks if you can you add another token
isSlotOpen :: [[Int]] -> Int -> Bool
isSlotOpen bd i = elem 0 col
    where col = bd !! i       

--return number of columns
numSlot :: [[Int]] -> Int
numSlot bd = length bd

--check if there are no more empty spaces
isFull :: [[Int]] -> Bool
isFull bd = not (elem 0 (concat bd))






--taking turns
changeTurn :: Int -> Int
changeTurn turn
     |turn == 1 = mkOpponent
     |otherwise = mkPlayer

slotHeight :: [[Int]] -> Int
slotHeight bd
    -- Height is zero if the board is empty
    | (length bd == 0) = 0
    -- Count the number of elements in the first column
    | otherwise = length (bd !! 0) 
    
--  Gets the total amount of spaces available in the board bd
getTotal :: [[Int]] -> Int
getTotal bd = (numSlot bd) * (slotHeight bd)




