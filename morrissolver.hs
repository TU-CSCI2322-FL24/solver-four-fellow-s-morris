{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Redundant return" #-}
module Morrissolver where
import Data.Char
import System.IO

import System.Directory (renameFile)

data Player = B | W | O deriving (Eq, Show)

data Turn = Place | Remove
--Move is a tuple of the 2 points that are changed in a move (ex: moving a piece off of one point 
--and onto another). Returns true if that move will give the current player a morris
--ReMove is for removing a piece if you have a morris

type Winner = Maybe Player
--Nothing if there's a tie

type Point = (Int, Int)
--Piece is nothing if no piece is on that space. The list of points contains adjacent points.

type Board = [Place]

--Point is nothing if the piece is removed.

type Place = (Point, Player)

type Remove = Point


allPoints :: [Point]
allPoints = [(1,7), (1,4), (1,1), (2,6), (2,4), (2,2), (3,5),
            (3,4), (3,3), (4,7), (4,5), (4,3), (4,1), (5,5),
            (5,4), (5,3), (6,6), (6,4), (6,2), (7,7), (7,4), (7,1)]

orderedPoints :: [Point]
orderedPoints = [(1,7), (4,7), (7,7), (2,6), (6,6), (3,5), (4,5), (5,5),
                 (1,4), (2,4), (3,4), (5,4), (6,4), (7,4), (3,3), (4,3), (5,3),
                 (2,2), (6,2), (1,1), (4,1), (7,1)]

makeBoard :: [Point] -> Board
makeBoard allPoints = [(x, O) | x <- allPoints]

isLegalMove :: Board -> Place -> Bool
isLegalMove board move = move `elem` board

legalMoves :: Board -> [Place]
legalMoves board = [(pt, ply) | (pt,ply) <- board, ply == O]

--Need to add error checking to make sure point is a legal point and on an open space in the board
makeMove :: Board -> Player -> Point -> Turn -> Board
makeMove board player point turn =
    map (\(pts, ply) -> if pts == point
                        then (pts, player)
                        else (pts, ply)) board

boardToString :: String -> Board -> String
boardToString boardString board =
    let aux :: String -> [Point] -> String
        aux [] _ = []
        aux (x:xs) [] = x : aux xs []
        aux (x:xs) (p:ps) =
            if x `elem` "BWO"
                then case lookupPlayer board p of
                        B -> 'B' : aux xs ps
                        W -> 'W' : aux xs ps
                        O -> 'O' : aux xs ps
                else x : aux xs (p:ps)
    in aux boardString orderedPoints
    
getBoardString :: IO String
getBoardString = readFile "Board.txt"

lookupPlayer :: Board -> Point -> Player
lookupPlayer [] _ = O  -- Default to O if no player is on a point
lookupPlayer ((pt, player):xs) point
    | pt == point = player
    | otherwise = lookupPlayer xs point

updateBoardPrint :: Board -> IO String
updateBoardPrint board = do
    boardString <- getBoardString
    let updatedStr = boardToString boardString board
    writeFile "Board.txt" updatedStr
    return updatedStr
