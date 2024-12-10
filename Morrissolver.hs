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

--data Result = OnGoing | Tie | Win Player deriving (Eq, Show)

--Place: where the players piece are curently at
-- Move: where the player is moveing a piece from one point to another 
-- Remove: feature removing opponets piece after they have made a mill
data Winner = Tie | Win Player deriving (Eq, Show)

data Result = Ongoing | Over Winner deriving (Eq, Show)

--Nothing if there's a tie
    -- what is it for ongoing?

type Point = (Int, Int)
----Piece is nothing if no piece is on that space. The list of points contains adjacent points.

--State of the board 

type Board = [Place]

-- Point is location of a piece on the board 
type Place = (Point, Maybe Player)

type Rating = Int

--type Remove = Point

-- board -- current player, have all 9 pieces been placed (True  or False)
-- Phase 1, normal, how many pieces each player has left
-- Phase 1/3, removing, with how may pieces each player has left 
-- Phase 2/3, normal or removing
-- Phase 2/3, removing
--Int tells us how many pieces we have 
--Bool is if last piece made a mill or not and then we wwant to remove
type Phase = Int

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
gameWinner (board, player, _, _)=
    let playerPieces =  length [ pos| (pos, ply) <- board, ply == Just (opponent player)]
        anyLegalMoves = length (legalPlaces board)
    in if playerPieces <= 2 || anyLegalMoves < 1 then Over (Win player) else Ongoing

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
validMoves place@(point, _) game@(board, player, phase, _, _)
  | phase == 3 = zipFly player game
  | phase == 2 =
      let x = fst point
          y = snd point
          possibleMoves = [(x, y + 1), (x, y - 1), (x - 1, y), (x + 1, y)]
          currentSpaces = getPlayerPlaces game (Just player)
          filteredMoves = filter
            (\point -> point `elem` allPoints && isOpen (point, lookupPlayer board point))
            possibleMoves
      in [Move ((x, y), z) | z <- filteredMoves]
  | otherwise = []



zipFly :: Player -> Game -> [Action]
zipFly player game =
    let playerSpaces = getPlayerPlaces game (Just player)
        emptySpaces = getPlayerPlaces game Nothing
    in [Move (fst x, fst y) | x <- playerSpaces, y <- emptySpaces]

--How to incorporate removes into the list of acctions?
legalActions :: Game -> [Action]
legalActions game@(board, player, phase, _, _) =
    if phase == 1 then [Put l | l <- allPoints \\ map fst board]
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
makeMove :: Game -> Action -> Game
makeMove game action =
    let board = getBoard game
        player = getPlayer game
        openBoard = getEmptyPlaces game
    in case action of
        --instead of using map use concat to remove from list
        --need to not use O
        Put point ->
            let newBoard = map (\(pts, Just ply) -> if pts == point then (pts, Just player) else (pts, Just ply)) openBoard
            in  (newBoard, opponent player, getPhase game, isMill point newBoard (Just player))
        Remove point ->
            let newBoard = map (\(pts, p) -> if pts == point then (pts, Nothing) else (pts,p)) openBoard
            in  (newBoard, opponent player, getPhase game, getMill game)
        Move (point1, point2) ->
            let putGame = makeMove game (Put point2)
                newGame = makeMove putGame (Remove point1)
                newBoard = getBoard newGame
                hasMill = isMill point2 newBoard (Just player)
            in  (newBoard, opponent player, getPhase game, hasMill)

--Need to adjust types
allPossibleMoves :: Game -> ([Action], [Action])
allPossibleMoves game =
    let player = getPlayer game
        board = getBoard game
        moves = legalActions game
        removes = [Remove (fst pieces) | pieces <- board, snd pieces == Just (opponent player)]
    in (moves, removes)

whoWillWin :: Game -> Winner
whoWillWin game@(board,player,_,_,_)  =
    case gameWinner game of
        Just winner -> Just winner
        Nothing ->
            let board = getBoard game
                moves = fst $ allPossibleMoves game
                newGames = [makeMove game move | move <- moves]
                millGames = filter getMill newGames
                removedGames = concat [snd (allPossibleMoves g) | g <- millGames]
                winners = map (\newGame -> whoWillWin newGame (opponent player)) newGames
            in if Just player `elem` winners
                then Just player
                else if all (== Just (opponent player)) winners
                    then Just (opponent player) 
                    else Nothing-}
bestMoveFor:: Player -> [(Winner, Action)] -> Action
bestMoveFor player winMoves=
    case (lookup (Win player) winMoves, lookup Tie winMoves ) of
        (Just winMove, _) -> winMove --fill in the rest later 

bestFor :: Player -> [Winner] -> Winner
bestFor player winners
    | Win player `elem` winners = Win player
    | Tie `elem` winners        = Tie
    | otherwise                 = Win (opponent player)

helper :: [(a,b)] -> [b]
helper lst = [snd b | b <- lst]

bestMove :: (Action,Game) -> Action
bestMove (mv,game@(brd, pl, phase, remove, turnC)) =
        case gameWinner game of
            Over winner -> mv
            Ongoing ->
                let moves = allPossibleMoves game
                    newGames = [(move, makeMove game move) | move <- moves]
                    bests = map bestMove newGames
                    winners = [(whoWillWin game, move)| (move, game) <- newGames ]
                in bestMoveFor pl winners

playerCounter :: Player -> Board-> Int
playerCounter player board =
    let numericBoard = filter  (\(pt, plyr) ->  plyr == Just player) board
    in length (numericBoard)

{-countMills :: Game -> Int
countMills game@(board, player, _, _) = 
    let millCount = -}

rateGame :: Game -> Rating
rateGame game@(board,player,_,_,_) = playerCounter W board - playerCounter B board


lookupPlayer :: Board -> Point -> Maybe Player
lookupPlayer [] _ = Nothing  -- Default to Nothing if no player is on a point
lookupPlayer ((pt, player):xs) point
    | pt == point = player
    | otherwise = lookupPlayer xs point


updateBoardPrint :: Board -> IO String
updateBoardPrint board = do
    boardString <- getBoardString
    let updatedStr = boardToString boardString board
    writeFile "Board.txt" updatedStr
    return updatedStr