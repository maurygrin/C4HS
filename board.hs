-- Mauricio Roberto Hidalgo & Mark Nunez 
-- Programming Assignment 3
-- Haskell
-- CS3360 TR 10:30 - 11:50 
-- 12/06/18

module Board(mkBoard, mkPlayer, mkOpponent, changeTurn, dropInSlot,isSlotOpen,numSlot,isFull, isWonBy, colHeight,boardToStr, changeTurn) where

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
getCol bd i = bd !! i

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

--3. (25 points) Determining the outcome-----------------------------------------

-- checks to see if player won
isWonBy:: [[Int]] -> Int -> Bool
isWonBy bd p = isWonByHelper bd p 0

--  isWonBy Helper
isWonByHelper :: [[Int]] -> Int -> Int -> Bool
isWonByHelper bd p pos
    | pos == (getAS bd) = False
    | winSequence bd p pos = True
    | otherwise = isWonByHelper bd p (pos+1)
     

--  Gets the total amount of avaible spaces
getAS :: [[Int]] -> Int
getAS bd = (numSlot bd) * (colHeight bd)

-- returns the height of the boards column
colHeight :: [[Int]] -> Int
colHeight bd
    | (length bd == 0) = 0
    | otherwise = length (bd !! 0) 

winSequence :: [[Int]] -> Int -> Int -> Bool
winSequence bd p index
    | indexOOB bd i j = False
    | (not (getTokenPos bd i j == p)) = False
    | (isWon == True) = True
    | otherwise = winSequence bd p (index + 1)
    where col = convertItoCol bd index;
          row = convertItoRow bd index;
          countRight = count bd col row 1 0 p 4;
          countLeft = count bd col row (-1) 0 p 4;
          countUp = count bd col row 0 1 p 4;
          countDown = count bd col row 0 (-1) p 4;
          countDiagonalUpLeft = count bd col row (-1) 1 p 4;
          countDiagonalDownLeft = count bd col row (-1) (-1) p 4;
          countDiagonalUpRight = count bd col row 1 1 p 4;
          countDiagonalDownRight = count bd col row 1 (-1) p 4;
          isWon = (countLeft == 3) || (countRight == 3) ||
                  (countUp == 3) || (countDown == 3) || 
                  (countDiagonalUpLeft == 3) || 
                  (countDiagonaldownLeft == 3) ||
                  (countDiagonalUpRight == 3) || 
                  (countDiagonalDownRight == 3);

-- index out of bounds check
indexOOB :: [[Int]] -> Int -> Int -> Bool
indexOOB bd i j 
    | i < 0 = True
    | j < 0 = True
    | i >= numSlot bd = True
    | j >= colHeight bd = True
    | otherwise = False

getTokenPos :: [[Int]] -> Int -> Int -> Int
getTokenPos bd i j 
    | indexOOB bd i j = -1
    | otherwise = (bd !! i) !! j
    

-- Converts an index to a column number of board bd
convertItoCol :: [[Int]] -> Int -> Int
convertItoCol bd i = mod i (numSlot bd)

-- Converts an index to a row number of board bd
convertItoRow :: [[Int]] -> Int -> Int
convertItoRow bd i = quot i ((colHeight bd) + 1)

  
--count the tokens of the current player
count bd i j i2 j2 p max
    | (max == 1) = 0
    | (getTokenPos bd col row == p) = 1 + callAgain
    | otherwise = callAgain
    where col = (i + i2);
          row = (j + j2)
          callAgain = count bd col row i2 j2 p (max - 1)

--4. (12 points) Converting a board to a string for printing-----------------------------------


--  Return a string representation of a board bd. 
boardToStr :: [[Int]] -> (Int -> String) -> String
boardToStr bd playerToChar = boardToStrHelper bd playerToChar 0

-- boardToStr Helper
boardToStrHelper :: [[Int]] -> (Int -> String) -> Int -> String
boardToStrHelper bd playerToChar i
    | (col == (totalCol - 1)) = tokenString ++ nextLine ++ callAgain
    | (i == numOfTokens) = ""
    | otherwise = tokenString ++ callAgain
    where callAgain = boardToStrHelper bd playerToChar (i + 1);
          totalCol = numSlot bd;
          numOfTokens = getAS bd;
          isLast = i == (totalAS - 1);
          nextLine = if (isLast) then "" else "\n";
          col = convertItoCol bd i;
          row = convertItoRow bd i;
          token = getTokenPos bd col row
          tokenString = playerToChar token






--taking turns
changeTurn :: Int -> Int
changeTurn turn
     |turn == 1 = mkOpponent
     |otherwise = mkPlayer





