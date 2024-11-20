{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Redundant return" #-}
module Morrissolver where
import Data.Char
import System.IO

import System.Directory (renameFile)

data Player = B | W | O deriving (Eq, Show)

--data Turn = Place | Remove deriving (Eq, Show)
data Action = Put Point | Move (Point, Point) | Remove Point deriving (Eq, Show)
--Move is a tuple of the 2 points that are changed in a move (ex: moving a piece off of one point 
--and onto another). Returns true if that move will give the current player a morris
--ReMove is for removing a piece if you have a morris

data Result = OnGoing | Tie | Win Player deriving (Eq, Show)

--Place: where the players piece are curently at
-- Move: where the player is moveing a piece from one point to another 
-- Remove: feature removing opponets piece after they have made a mill



type Winner = Maybe Player
--Nothing if there's a tie

type Point = (Int, Int)
----Piece is nothing if no piece is on that space. The list of points contains adjacent points.

--State of the board 
type Board = [(Point, Player)]

-- Point is location of a piece on the board 
type Place = (Point, Player)

--type Remove = Point

-- board -- current player, have all 9 pieces been placed (True  or False)
-- Phase 1, normal, how many pieces each player has left
-- Phase 1, removing, with how may pieces each player has left 
-- Phase 2/3, normal or removing
-- Phase 2/3, removing
--Int tells us how many pieces we have 
--Bool is if last piece made a mill or not and then we wwant to remove
type Phase = Int

type Game = (Board, Player, Phase, Bool)


allPoints :: [Point]
allPoints = [(1,7), (1,4), (1,1), (2,6), (2,4), (2,2), (3,5),
            (3,4), (3,3), (4,7), (4,5), (4,3), (4,1), (5,5),
            (5,4), (5,3), (6,6), (6,4), (6,2), (7,7), (7,4), (7,1)]


-- there must be an easier way then hardcoding all the 
-- when have a mill can remove those edges 
-- 
allEdges :: [(Point, Point)]
allEdges =
    let forward = [
         ((1,1), (1,4)), ((1,4), (1,7)),
         ((7,1), (7,4)), ((7,4), (7,7)),
         ((1,1), (4,1)), ((4,1), (7,1)),
         ((1,7), (4,7)), ((4,7), (7,7)),
         ((2,2), (2,4)), ((2,4), (2,6)),
         ((6,2), (6,4)), ((6,4), (6,6)),
         ((2,2), (4,2)), ((4,2), (6,2)),
         ((2,6), (4,6)), ((4,6), (6,6)),
         ((3,3), (3,4)), ((3,4), (3,5)),
         ((5,3), (5,4)), ((5,4), (5,5)),
         ((3,3), (4,3)), ((4,3), (5,3)),
         ((3,5), (4,5)), ((4,5), (5,5)),
         ((1,4), (2,4)), ((2,4), (3,4)),
         ((4,1), (4,2)), ((4,2), (4,3)),
         ((4,7), (4,6)), ((4,6), (4,5)),
         ((7,4), (6,4)), ((6,4), (5,4))]
    in forward ++ [(b,a) | (a,b) <- forward]


allMills:: [[Point]]
allMills =
    let forward = [
            [(1,1), (1,4), (1,7)],
            [(2,2), (2,4), (2,6)],
            [(3,3), (3,4), (3,5)],
            [(4,1), (4,2), (4,3)],
            [(4,5), (4,6), (4,7)],
            [(5,3), (5,4), (5,5)],
            [(6,2), (6,4), (6,6)],
            [(7,1), (7,4), (7,7)],
            [(1,1), (4,1), (7,1)],
            [(2,2), (4,2), (6,2)],
            [(3,3), (4,3), (5,3)],
            [(1,4), (2,4), (3,4)],
            [(5,4), (6,4), (7,4)],
            [(3,5), (4,5), (5,5)],
            [(2,6), (4,6), (6,6)],
            [(1,7), (4,7), (7,7)] ]
    in forward ++ [(b,a) | (a,b) <- forward]


-- need to keep track of mills
-- wanna check after every turn if we have a mill
-- can do this by checking if there are two edges that are conected 
    -- so if the y in one cordiante is the same as the x in the other and vice versa 
    -- lookUp point (x, y) && lookup 
--mill :: Player -> Board -> Bool
--mill player board =
    --let playerPiecesOnBoard = [ pos| (pos, ply) <- board, ply == player]
    --in  any (\mill -> length (playerPositions `intersect` mill) == length mill) allMills

ismill :: Point -> Board -> Player -> Bool
ismill pieceLoc board pl =
    let adjacentMills = [mill | mill <- allMills, pieceLoc `elem` mill]
        playerFilled mill = all (\point -> lookup point board == Just pl) mill
    in any playerFilled adjacentMills



--determine who is gonna win the game 
--either the oppnoet only has 2 pices left or they have no more legal moves 
gameWinner :: Game -> Winner
gameWinner (board, player, _, _)=
    let playerPieces =  length [ pos| (pos, ply) <- board, ply == opponent player]
        anyLegalMoves = length (legalPlaces board)
    in if playerPieces <= 2 || anyLegalMoves < 1 then Just player else Nothing


opponent :: Player -> Player
opponent B = W
opponent W = B


-- want to count how many pices of each payer are on the board 
--pieces :: PLayer -> Borad -> 

-- have to replace the O with soemthing else
orderedPoints :: [Point]
orderedPoints = [(1,7), (4,7), (7,7), (2,6), (6,6), (3,5), (4,5), (5,5),
                 (1,4), (2,4), (3,4), (5,4), (6,4), (7,4), (3,3), (4,3), (5,3),
                 (2,2), (6,2), (1,1), (4,1), (7,1)]

makeBoard :: [Point] -> Board
makeBoard allPoints = [(x, O) | x <- allPoints]

getBoard :: Game -> Board
getBoard (a,_,_,_) = a

getPhase :: Game -> Phase
getPhase (_,_,a,_) = a

getPlayer :: Game -> Phase
getPlayer (_,a,_,_) = a

isLegalMove :: Board -> Place -> Bool
isLegalMove board move = move `elem` board

isOpen :: Place -> Bool
isOpen ((a,b), c) = fromMaybe True c

--Create function to check if a player is black or white, or one that returns the type of a player
getPlayerPlaces :: Game -> Player -> [Place]
blackPlaces game player =
    let board = getBoard game
    in filter (\(a,b) -> b == B) board

--Not technically a list of moves, since moves are (point,point) and this is a list of points, 
--to get it into a list of moves, it would only require adding the starting point passed in
validMoves :: Place -> Game -> [Moves]
validMoves place game =
    let board = getBoard game
    in if getPhase game == 3
        then filter (\(point, plyr) -> isOpen point) board
        else if getPhase game == 2
            then let point = fst place
                     x = fst point
                     y = snd point
                     possibleMoves = [(x,y+1), (x,y-1), (x-1,y), (x+1,y)]
                 in filter (\point -> point `elem` allPoints && isOpen point) possibleMoves
        else []

--How to incorporate removes into the list of actions?
legalActions :: Game -> [Action]
legalActions game =
    let board = getBoard game
        player = getPlayer game
    in  if getPhase game == 1 then [Put l | l <- allPoints \\ map fst board]
        else [validMoves place | place <- getPlayerPlaces game player]


--what should i put in other than O 
legalPlaces :: Board -> [Place]
legalPlaces board = [place | place <- board, isOpen place]


--Need to add error checking to make sure point is a legal point and on an open space in the board
--Is there a way to set a default value?
--Do we need to change this in order to make it incorporate a game return as opposed to board?
--We need to rewrite this function tbh
makeMove :: Game -> Move -> Action -> Game
makeMove game point action =
    let board = getBoard game
        player = getPlayer game
        openBoard = legalMoves board
    in case action of
        --instead of using map use concat to remove from list 
        --need to not use O
        Place -> map (\(pts, ply) -> if pts == point then (pts, player) else (pts,ply)) openBoard
        Remove -> map (\(pts, p) -> if pts == point then (pts, O) else (pts,p)) openBoard

--Need to adjust types
allPossibleMoves :: Game -> (Removes, Move)
allPossibleMoves game =
    let player = getPlayer game
        removes = [pieces | pieces <- getBoard board, snd pieces == oponent player]
        moves = legalActions game
    in (removes, moves)


whoWillWin :: Game -> Player -> Winner
whoWillWin game =
    let board = getBoard game
        player = getPlayer game
        moves = allPossibleMoves game
        newGames = [makeMove game move | allPossibleMoves game]
        winners = map gameWinner newGames
        whoWon = filter isNothing winners
        wasWinner = map isJust winners

    in  if foldr (||) wasWinner then fromMaybe True fst whoWon
        else map whoWillWin newBoards


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
