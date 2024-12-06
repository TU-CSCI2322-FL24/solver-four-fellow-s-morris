{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Redundant return" #-}
module Morrissolver where
import Data.Char
import System.IO

import System.Directory (renameFile)
import Data.Maybe
import Data.List
import Data.List.Split

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
-- Phase 1/3, removing, with how may pieces each player has left 
-- Phase 2/3, normal or removing
-- Phase 2/3, removing
--Int tells us how many pieces we have 
--Bool is if last piece made a mill or not and then we wwant to remove
type Phase = Int
type TurnCounter = Int

type Game = (Board, Player, Phase, Bool, TurnCounter)


allPoints :: [Point]
allPoints = [(1,7), (4,7), (7,7), (2,6), (6,6), (3,5), (4,5), (5,5),
                 (1,4), (2,4), (3,4), (5,4), (6,4), (7,4), (3,3), (4,3), (5,3),
                 (2,2), (6,2), (1,1), (4,1), (7,1)]

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

newGame :: Player -> Game
newGame player = (makeBoard allPoints, player, 1, False, 0)

isMill :: Point -> Board -> Maybe Player -> Bool
isMill pieceLoc board pl =
    let adjacentMills = [mill | mill <- allMills, pieceLoc `elem` mill]
        playerFilled = all (\l -> lookup l board == Just pl)
    in any playerFilled adjacentMills

--determine who is gonna win the game 
--either the oppnoet only has 2 pices left or they have no more legal moves 
gameWinner :: Game -> Winner
gameWinner (board, player, _, _, _)=
    let playerPieces =  length [ pos| (pos, ply) <- board, ply == Just (opponent player)]
        anyLegalMoves = length (legalPlaces board)
    in if playerPieces <= 2 || anyLegalMoves < 1 then Just player else Nothing


opponent :: Player -> Player
opponent B = W
opponent W = B


makeBoard :: [Point] -> Board
makeBoard allPoints = [(x, Nothing) | x <- allPoints]

getBoard :: Game -> Board
getBoard (a,_,_,_,_) = a

getPhase :: Game -> Phase
getPhase (_,_,a,_,_) = a

getPlayer :: Game -> Player
getPlayer (_,a,_,_,_) = a

getMill :: Game -> Bool
getMill (_,_,_,a,_) = a

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


-- did pattern match instead as well as included the error handling \
-- add +1 for every turn and once get to 200 the game should output end 
makeMove :: Game -> Action -> Game
makeMove game@(board, player, phase, True, 200) _ = error "Game is over!"
makeMove game@(board, player, phase, True, turns) (Remove point) =
    let newBoard = map (\(pts, p) -> if pts == point then (pts, Nothing) else (pts, p)) board
    in (newBoard, player, phase, False, turns)
makeMove game@(board, player, phase, True, turns) _ = error "Must be a Remove action"
makeMove game@(board, player, phase, False, turns) (Remove point) = error "Cannot perform a Remove action in this phase"

makeMove game@(board, player, 1, False, turns) (Put point) =
    let newBoard = map (\(pts, p) -> if pts == point then (pts, Just player) else (pts, p)) board
        nextPhase = if turns >= 18 then 2 else 1
    in (newBoard, opponent player, nextPhase, isMill point newBoard (Just player), turns + 1)

makeMove game@(board, player, 2, False, turns) (Move (from, to)) =
    if not (isLegalMove board (to, Nothing)) then error "Illegal move"
    else
        let newBoard = map (\(pts, p) -> if pts == from then (pts, Nothing) else if pts == to then (pts, Just player) else (pts, p)) board
        in (newBoard, opponent player, 2, isMill to newBoard (Just player), turns)

makeMove _ _ = error "Invalid action"

--Need to adjust types
allPossibleMoves :: Game -> [Action]
allPossibleMoves game@(board, player, phase, mill, _) = if mill then legalActions game else
    [Remove (fst pieces) | pieces <- board, snd pieces == Just (opponent player)]

whoWillWin :: Game -> Player -> Winner
whoWillWin game player =
    case gameWinner game of
        Just winner -> Just winner
        Nothing ->
            let board = getBoard game
                moves = allPossibleMoves game
                moveGames = [makeMove game move | move <- moves]
                millGames = filter getMill moveGames
                removes = concat [allPossibleMoves g | g <- millGames]
                removeGames = [makeMove games remove | remove <- removes, games <- millGames]
                newGames = moveGames ++ removeGames
                winners = map (\newGame -> whoWillWin newGame (opponent player)) newGames
            in if Just player `elem` winners
                then Just player
                else if all (== Just (opponent player)) winners
                    then Just $ opponent player
                    else Nothing --change to recursive call

lookupPlayer :: Board -> Point -> Maybe Player
lookupPlayer [] _ = Nothing  -- Default to Nothing if no player is on a point
lookupPlayer ((pt, player):xs) point
    | pt == point = player
    | otherwise = lookupPlayer xs point

--Printing

playerString :: Maybe Player -> String
playerString (Just B) = "B"
playerString (Just W) = "W"
playerString Nothing  = "O"

stringPlayer :: String -> Maybe Player
stringPlayer "B" = Just B
stringPlayer "W" = Just W
stringPlayer "O" = Nothing
stringPlayer x = error (x ++ " is not a valid player (did you make sure that the string is uppercase?)")

playerChar :: Maybe Player -> Char
playerChar (Just B) = 'B'
playerChar (Just W) = 'W'
playerChar Nothing  = 'O'

removeMaybe :: Maybe Player -> Player
removeMaybe (Just B) = B
removeMaybe (Just W) = W
removeMaybe _ = error "Needs to be an actual player"

--String that looks like the game in progress.
prettyPrint :: Game -> String
prettyPrint (board, _, _, _, _) = "7 " ++ playerString (lookupPlayer board (1,7)) ++ "        " ++ playerString (lookupPlayer board (4,7)) ++ "        " ++ playerString (lookupPlayer board (7,7)) ++ "\n6    " ++ playerString (lookupPlayer board (2,6)) ++ "     " ++ playerString (lookupPlayer board (4,6)) ++ "     " ++ playerString (lookupPlayer board (6,6)) ++ "\n5       " ++ playerString (lookupPlayer board (3,5)) ++ "  " ++ playerString (lookupPlayer board (4,5)) ++ "  " ++ playerString (lookupPlayer board (5,5)) ++ "\n4 " ++ playerString (lookupPlayer board (1,4)) ++ "  " ++ playerString (lookupPlayer board (2,4)) ++ "  " ++ playerString (lookupPlayer board (3,4)) ++ "     " ++ playerString (lookupPlayer board (5,4)) ++ "  " ++ playerString (lookupPlayer board (6,4)) ++ "  " ++ playerString (lookupPlayer board (7,4)) ++ "\n3       " ++ playerString (lookupPlayer board (3,3)) ++ "  " ++ playerString (lookupPlayer board (4,3)) ++ "  " ++ playerString (lookupPlayer board (5,3)) ++ "\n2    " ++ playerString (lookupPlayer board (2,2)) ++ "     " ++ playerString (lookupPlayer board (4,2)) ++ "     " ++ playerString (lookupPlayer board (6,2)) ++ "\n1 " ++ playerString (lookupPlayer board (1,1)) ++ "        " ++ playerString (lookupPlayer board (4,1)) ++ "        " ++ playerString (lookupPlayer board (7,1)) ++ "\n  1  2  3  4  5  6  7\n"

--For testing IO in GHCi
testOut :: Game -> IO ()
testOut g = putStrLn (prettyPrint g)

--String that is put in file
pickle :: Game -> String
pickle (board, player, phase, mill, turn) = 
    [playerChar (snd s) | s <- board] ++ " " 
    ++ playerString (Just player) ++ " " 
    ++ show phase ++ " " 
    ++ show mill ++ " "
    ++ show turn

--Game that is read out
unpickle :: String -> Game
unpickle string = 
    let [b, pl, ph, m, t] = splitOn " " string
        board  = zip allPoints [stringPlayer [x] | x <- b]
        player = stringPlayer pl
        phase  = read ph :: Int
        mill   = read m :: Bool
        turn   = read t :: Int
    in  (board, removeMaybe player, phase, mill, turn)

getBoardString :: IO String
getBoardString = readFile "Board.txt"