-- Mauricio Roberto Hidalgo & Mark Nunez 
-- Programming Assignment 3
-- Haskell
-- CS3360 TR 10:30 - 11:50 
-- 12/06/18

import Data.List
import Data.Maybe 
import System.IO 
import System.Random
import Board
 
main = do
    let bd = mkBoard 7 6
    let p = mkPlayer
    putStrLn (boardToStr bd playerToChar)
    putStrLn ""
    do game bd p

game :: [[Int]] -> Int -> IO()
game bd p = do
    col <- readSlot bd p

    if not(col == (-1))
    then do

        let updatedBoard = dropInSlot bd col p

        let drawnBoard = (boardToStr updatedBoard playerToChar)

        if(isWonBy updatedBoard p)
        then do 
            putStrLn drawnBoard
            putStr "Player "
            printWinner p
            putStrLn " won!"

        else do
            putStrLn drawnBoard
            putStrLn ""
            game updatedBoard (changePlayer p)

    else do 
        putStrLn "\nGood bye!\n"

changeTurn :: Bool -> Bool
changeTurn current
    | current = False
    | otherwise = True

readSlot :: [[Int]] -> Int -> IO(Int)        
readSlot bd p = do
    putStrLn ("Player " ++ playerToChar p ++ "'s turn")
    putStr ("Select 0-6 or enter -1 to end: ")
    line <- getLine
    let input = reads line :: [(Int, String)] in
      if length input == 0
      then readSlot'
      else let (col, _) = head input in
        if (col <= numSlot bd) && 
           (col >= 0 && col <= 6 ) && 
           (isSlotOpen bd col)
        then return col
        else do
            if (col == (-1))
            then end
            else do readSlot'
    where
      readSlot' = do
        putStrLn "Invalid index"
        readSlot bd p
      end = do
        return (-1)

-- prints the player as winner
printWinner :: Int -> IO()     
printWinner player = do
    if player == 1
    then putStr "1 (O)"
    else putStr "2 (X)"
        
playerToChar :: Int -> String
playerToChar player
 | (player == 1) = "O "
 | (player == 2) = "X "
 | otherwise = ". "