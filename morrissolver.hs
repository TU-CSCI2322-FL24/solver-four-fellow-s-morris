data Player = Black | White

Winner = Maybe Player
//Nothing if there’s a tie

Mill = Maybe Player
//Nothing if no player has a mill

Mills = [Mill]

Point = (Maybe Piece, [Point])
//Piece is nothing if no piece is on that space. The list of points contains adjacent points.

Game = [Point]

Piece = (Player, Maybe Point)
//Point is nothing if the piece is removed.

Pieces = [Piece]

data Turn = Move ((Point, Point), Bool) | ReMove = (Point, Piece)
//Move is a tuple of the 2 points that are changed in a move (ex: moving a piece off of one point 
and onto another). Returns true if that move will give the current player a morris
//ReMove is for removing a piece if you have a morris

legalMoves :: Game -> [Move]

makeMove :: Game -> Player -> Move -> Game

state :: Game -> String

