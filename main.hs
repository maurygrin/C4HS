-- Mauricio Roberto Hidalgo & Mark Hiram Nunez
-- Programming Assignment 3
-- Haskell
-- CS3360 TR 10:30 - 11:50
-- 12/06/18

import Data.List
import System.IO
import Board

-- Main of the proyect that starts the game.
main = do
     let bd = mkBoard 7 6
     let p = mkPlayer
 
-- Return a char representation of a player p.
playerToChar :: Int -> String
playerToChar p 
     | (p == 1) = "O "
     | (p == 2) = "X "
     | otherwise = ". "

-- Read a 1-based index of an open slot of a 
-- board bd for a player p to drop his disc.
readSlot :: [[Int]] -> Int -> IO(Int)
readSlot bd p = do
     putStrLn(playerToChar p ++ "'s turn")
     putStr("Enter 1-7 .. -1 to end: ")
     l <= getLine
     let input = reads l :: [(Int,String)] in 
          if length input == 0
          then invalidSlot
          else let(col, _) = head input in
               if(col <= numSlot bd) && (col >= 1 && col <= 7)
               && (isSlotOpen bd col) 
               then return col
               else do
               if(col == (-1))
               then end
               else do invalidSlot 
     where
          invalidSlot = do
               putStrLn "Invalid slot"
               readSlot bd p
          end = do 
               return (-1)

-- Starts a game with another player.
playGame :: [[Int]] -> Int -> IO()
playGame bd p = do
     col <- readSlot bd p 
     if not(col == (-1))
     then do
          let newBoard = dropInSlot bd col p
          let stringBoard = (boardToStr newBoard playerToChar)
          if(playerWin newBoard p)
          then do
               putStrLn stringBoard
               showWin p 
               putStrLn " is the winner"
          else do 
               putStrLn stringBoard
               putStrLn ""
               playGame newBoard (changeTurn p)
     else do 
          putStrLn "\nThanks for playing\n"

-- Shows who won the game.
showWin :: Int -> IO()
showWin p = do
     if p == 1
     then putStr "1"
     else putStr "2"

-- Change current turn.
changeTurn :: Bool -> Bool
changeTurn currentP
     | currentP = False
     | otherwise = True