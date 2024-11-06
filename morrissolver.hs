data Player = Black | White 

data Turn = Place | Remove 
--Move is a tuple of the 2 points that are changed in a move (ex: moving a piece off of one point 
--and onto another). Returns true if that move will give the current player a morris
--ReMove is for removing a piece if you have a morris

data Move = Up | Down | Left | Right

type Winner = Maybe Player
--Nothing if there's a tie

type Mill = Maybe Player
--Nothing if no player has a mill

type Mills = [Mill]

type Point = (Int, Int, Maybe Player)
--Piece is nothing if no piece is on that space. The list of points contains adjacent points.

type Game = [Point]

type Piece = (Player, Maybe Point)
--Point is nothing if the piece is removed.

type Pieces = [Piece]

type Place = (Point, Point, Bool)

type Remove = (Point, Point)

legalMoves :: Game -> [Move]

makeMove :: Game -> Player -> Turn -> Game

state :: Game -> String

