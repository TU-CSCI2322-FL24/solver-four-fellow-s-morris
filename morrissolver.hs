data Player = Black | White 

data Turn = Place | Remove 
--Move is a tuple of the 2 points that are changed in a move (ex: moving a piece off of one point 
--and onto another). Returns true if that move will give the current player a morris
--ReMove is for removing a piece if you have a morris

data Move = Up | Down | Left | Right

type Winner = Maybe Player
--Nothing if there's a tie

type Point = (Int, Int)
--Piece is nothing if no piece is on that space. The list of points contains adjacent points.

type Filled = [(Point, Maybe Player)]

type Board = [(Point, Maybe Piece)]

type Game = [Point]

type Piece = (Maybe Point, Player)
--Point is nothing if the piece is removed.

type Pieces = [Piece]

type Place = (Point, Point, Bool)

type Remove = (Point, Point)

legalMoves :: Game -> [Move]
legalMoves = undefined

makeMove :: Game -> Player -> Turn -> Game
makeMove = undefined

state :: Game -> String
