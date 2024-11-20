{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Redundant return" #-}
module Morrissolver where
import Data.Char
import System.IO

import System.Directory (renameFile)
import Data.Maybe
import Data.List

data Player = B | W deriving (Eq, Show)

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
type Board = [Place]

-- Point is location of a piece on the board 
type Place = (Point, Maybe Player)

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
allMills = [ [(1,1), (1,4), (1,7)],
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

-- need to keep track of mills
-- wanna check after every turn if we have a mill
-- can do this by checking if there are two edges that are conected 
    -- so if the y in one cordiante is the same as the x in the other and vice versa 
    -- lookUp point (x, y) && lookup 
mill :: Point -> Board -> Maybe Player -> Bool
mill pieceLoc board pl =
    let adjacentMills = [mill | mill <- allMills, pieceLoc `elem` mill]
        playerFilled = all (\l -> lookup l board == Just pl)
    in any playerFilled adjacentMills

--determine who is gonna win the game 
--either the oppnoet only has 2 pices left or they have no more legal moves 
gameWinner :: Game -> Winner
gameWinner (board, player, _, _)=
    let playerPieces =  length [ pos| (pos, ply) <- board, ply == Just (opponent player)]
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
makeBoard allPoints = [(x, Nothing) | x <- allPoints]

getBoard :: Game -> Board
getBoard (a,_,_,_) = a

getPhase :: Game -> Phase
getPhase (_,_,a,_) = a

getPlayer :: Game -> Player
getPlayer (_,a,_,_) = a

getMill :: Game -> Bool
getMill (_,_,_,a) = a

isLegalMove :: Board -> Place -> Bool
isLegalMove board move = move `elem` board

isOpen :: Place -> Bool
isOpen ((a,b), c) = isNothing c

--Create function to check if a player is black or white, or one that returns the type of a player
getPlayerPlaces :: Game -> Maybe Player -> [Place]
getPlayerPlaces game player =
    let board = getBoard game
    in filter (\(a,b) -> b == player) board

getEmptyPlaces :: Game -> [Place]
getEmptyPlaces game = getPlayerPlaces game Nothing

--Not technically a list of moves, since moves are (point,point) and this is a list of points, 
--to get it into a list of moves, it would only require adding the starting point passed in
validMoves :: Place -> Game -> [Action]
validMoves place game =
    let board = getBoard game
        player = getPlayer game
    in if getPhase game == 3
        then zipFly player game
        else if getPhase game == 2
            then
                let point = fst place
                    x = fst point
                    y = snd point
                    possibleMoves = [(x, y + 1), (x, y - 1), (x - 1, y), (x + 1, y)]
                    currentSpaces = getPlayerPlaces game (Just player)
                    filteredMoves = filter (\point -> point `elem` allPoints && isOpen (point, lookupPlayer board point)) possibleMoves
                in  [Move ((x,y), z) | z <- filteredMoves]
            else []


zipFly :: Player -> Game -> [Action]
zipFly player game =
    let playerSpaces = getPlayerPlaces game (Just player)
        emptySpaces = getPlayerPlaces game Nothing
    in [Move (fst x, fst y) | x <- playerSpaces, y <- emptySpaces]

--How to incorporate removes into the list of acctions?
legalActions :: Game -> [Action]
legalActions game =
    let board = getBoard game
        player = getPlayer game
    in  if getPhase game == 1 then [Put l | l <- allPoints \\ map fst board]
        else
            let place = getPlayerPlaces game (Just player)
                aux :: [Place] -> [Action]
                aux [] = []
                aux (plc:plcs) = validMoves plc game ++ aux plcs
            in  aux place

legalPlaces :: Board -> [Place]
legalPlaces board = [place | place <- board, isOpen place]


--Need to add error checking to make sure point is a legal point and on an open space in the board
--Is there a way to set a default value?
--Do we need to change this in order to make it incorporate a game return as opposed to board?
--We need to rewrite this function tbh



makeMove :: Game -> Point -> Action -> Game
makeMove game point action =
    let board = getBoard game
        player = getPlayer game
        openBoard = getEmptyPlaces game
    in case action of
        --instead of using map use concat to remove from list
        --need to not use O
        Put point -> 
            let newBoard = map (\(pts, Just ply) -> if pts == point then (pts, Just player) else (pts, Just ply)) openBoard
            in  (newBoard, opponent player, getPhase game, getMill game)
        Remove point -> 
            let newBoard = map (\(pts, p) -> if pts == point then (pts, Nothing) else (pts,p)) openBoard 
            in  (newBoard, opponent player, getPhase game, getMill game)

--Need to adjust types
allPossibleMoves :: Game -> ([Action], [Action])
allPossibleMoves game =
    let player = getPlayer game
        board = getBoard game
        moves = legalActions game
        removes = [Remove (fst pieces) | pieces <- board, snd pieces == Just (opponent player)]
    in (moves, removes)


whoWillWin :: Game -> Player -> Winner
whoWillWin game player =
    case gameWinner game of
        Just winner -> Just winner
        Nothing ->
            let board = getBoard game
                moves = fst $ allPossibleMoves game 
                removes = snd $ allPossibleMoves game
                newGames = [makeMove game point move | point <- map fst (getPlayerPlaces game (Just player)), move <- moves]
                winners = map (\newGame -> whoWillWin newGame (opponent player)) newGames
            in if Just player `elem` winners
                then Just player
                else if all (== Just (opponent player)) winners
                    then Just (opponent player) 
                    else Nothing



boardToString :: String -> Board -> String
boardToString boardString board =
    let aux :: String -> [Point] -> String
        aux [] _ = []
        aux (x:xs) [] = x : aux xs []
        aux (x:xs) (p:ps) =
            if x `elem` "BWO"
                then case lookupPlayer board p of
                    Just B  -> 'B' : aux xs ps
                    Just W  -> 'W' : aux xs ps
                    Nothing -> 'O' : aux xs ps
                else x : aux xs (p:ps)
    in aux boardString orderedPoints

getBoardString :: IO String
getBoardString = readFile "Board.txt"

lookupPlayer :: Board -> Point -> Maybe Player
lookupPlayer [] _ = Nothing  -- Default to O if no player is on a point
lookupPlayer ((pt, player):xs) point
    | pt == point = player
    | otherwise = lookupPlayer xs point


updateBoardPrint :: Board -> IO String
updateBoardPrint board = do
    boardString <- getBoardString
    let updatedStr = boardToString boardString board
    writeFile "Board.txt" updatedStr
    return updatedStr