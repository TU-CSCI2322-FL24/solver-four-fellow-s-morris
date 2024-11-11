data Player = B | W | O

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

allPoints = [(1,7), (1,4), (1,1), (2,6), (2,4), (2,2), (3,5), 
            (3,4), (4,7), (4,6), (4,5), (4,3), (4,2), (4,1), (5,5), 
            (5,4), (5,3), (6,6), (6,4), (6,2), (7,7), (7,4), (7,1)]


isLegalMove :: Board -> Place -> Bool
isLegalMove board move = move `elem` board

legalMoves :: Board -> [Place]
legalMoves = undefined

makeMove :: Board -> Player -> Turn -> Board
makeMove = undefined

state :: Board -> String
state = undefined
