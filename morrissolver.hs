data Player = B | W | O deriving (Eq, Show)

--data Turn = Place | Remove deriving (Eq, Show)
--Move is a tuple of the 2 points that are changed in a move (ex: moving a piece off of one point 
--and onto another). Returns true if that move will give the current player a morris
--ReMove is for removing a piece if you have a morris

data Result = OnGoing | Tie | Win Player deriving (Eq, Show)

--Place: where the players piece are curently at
-- Move: where the player is moveing a piece from one point to another 
-- Remove: feature removing opponets piece after they have made a mill
data PieceMove = Place Point | Move (Point, Point) | Remove Point deriving (Eq, Show)


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
type Game = (Board, Player, Int, Int, Bool)


allPoints :: [Point]
allPoints = [(1,7), (1,4), (1,1), (2,6), (2,4), (2,2), (3,5), 
            (3,4), (4,7), (4,6), (4,5), (4,3), (4,2), (4,1), (5,5), 
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

dicMills:: [[Point]]
dicMills = [ [(1,1), (1,4), (1,7)],
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
mill :: Player -> Board -> Bool
mill player board = 
    let playerPiecesOnBoard = [ pos| (pos, ply) <- board, ply == player]
    in  any (\mill -> length (intersect playerPositions mill) == length mill) dicMills

--determine who is gonna win the game 
--either the oppnoet only has 2 pices left or they have no more legal moves 
gameWinner :: Game -> Winner
gameWinner (board, player, _, _, _)= 
    let playerPieces =  length [ pos| (pos, ply) <- board, ply == opponent player]
        anyLegalMoves = length (legalPlaces board)
    in if playerPieces <= 2 || anyLegalMoves < 1 then Just player else Nothing 


opponent :: Player -> Player 
opponent B = W
opponent W = B


-- want to count how many pices of each payer are on the board 
--pieces :: PLayer -> Borad -> 


-- have to replace the O with soemthing else
makeBoard :: [Point] -> Board
makeBoard allPoints = [(x, O) | x <- allPoints]

isLegalMove :: Board -> Place -> Bool
isLegalMove board move = move `elem` board

--what should i put in other then O 
legalPlaces :: Board -> [Place]
legalPlaces board = [(pt, ply) | (pt,ply) <- board, ply == O]


--Need to add error checking to make sure point is a legal point and on an open space in the board
makeMove :: Board -> Player -> Point -> Turn -> Board
makeMove board player point turn = 
    let openBoard = legalMoves board
    in case turn of 
        --instead of using map use concat to remove from list 
        --need to not use O
        Place -> map (\(pts, ply) -> if pts == point then (pts, player) else (pts,ply)) openBoard
        Remove -> map (\(pts, p) -> if pts == point then (pts, O) else (pts,p)) openBoard


state :: Board -> String
state = undefined

