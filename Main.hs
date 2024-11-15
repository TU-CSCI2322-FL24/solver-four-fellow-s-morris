module Main where
import Data.Char
import System.IO
import Data.Maybe
import Text.Read
import Morrissolver
import Control.Monad

main :: IO ()
main = do
    resetBoard
    putStrLn "Select player color (B/W): "
    input <- getLine
    let playerCol = map toUpper input
    if testInput playerCol then applyInput playerCol defaultBoard
        else if playerCol /= "B" && playerCol /= "W"
        then do
            putStrLn (playerCol ++ " is not a valid color.")
            main
        else do 
            let enemyCol = if playerCol == "B" then "W" else "B"
            putStrLn ("You selected " ++ playerCol ++ "\n")
            placePhase Place (toPlayer playerCol) (toPlayer enemyCol) (toPlayer playerCol) 0 defaultBoard


placePhase :: Turn -> Player -> Player -> Player -> Int -> Board -> IO ()
placePhase turn player enemy actor count board = do
    if count > 8 then playPhase turn player enemy actor board else
        if actor == player then do
            putStrLn "\nPlace your piece [ (x, y) ]..."
            printBoard board
            inputPlace <- getLine
            let input = map toUpper inputPlace
                placement = parseTuple inputPlace
                legal = legalMoves board
            if testInput input then applyInput input board
            else if (placement, O) `elem` legal then do
                newBoard <- playPiece board player placement
                placePhase turn player enemy enemy count newBoard
            else do
                putStrLn ("Cannot place a piece at " ++ show placement)
                placePhase turn player enemy player count board
        else do
            let legal = fst (head (legalMoves board))
            putStrLn ("\nEnemy placed a piece on " ++ show legal)
            newBoard <- playPiece board enemy legal
            placePhase turn player enemy player (count + 1) newBoard

playPhase :: Turn -> Player -> Player -> Player -> Board -> IO ()
playPhase turn player enemy actor board = do
    putStrLn "Beginning playPhase\n" 
    printBoard board

playPiece :: Board -> Player -> Point -> IO Board
playPiece board player placement = do
    let newBoard = makeMove board player placement Place
    return newBoard

printBoard :: Board -> IO ()
printBoard board = do
    boardString <- getBoardString
    let updatedStr = boardToString boardString board
    putStrLn updatedStr

toPlayer :: String -> Player
toPlayer "B" = B
toPlayer "W" = W
toPlayer _ = O

parseTuple :: String -> Point
parseTuple s = 
    let trimmed = filter (`notElem` " ()") s
    in case break (== ',') trimmed of
        (aStr, ',' : bStr) -> 
            case (readMaybe aStr, readMaybe bStr) of
                (Just intA, Just intB) -> (intA, intB)
                _ -> error "Invalid format: could not parse integers."
        _ -> error "Invalid format: expected format (x, y)"

defBoard :: IO String
defBoard = readFile "DefaultBoard.txt"

resetBoard :: IO ()
resetBoard = do
    reset <- defBoard
    writeFile "Board.txt" reset 

quitInputs :: [String]
quitInputs = ["QUIT", "Q", "EXIT", "END"]

showBoardInputs :: [String]
showBoardInputs = ["BOARD", "STATE", "GAME", "G", "SHOW"]

allInputs :: [String]
allInputs = quitInputs ++ showBoardInputs

testInput :: String -> Bool
testInput input = input `elem` allInputs

applyInput :: String -> Board-> IO ()
applyInput input board =
    if input `elem` quitInputs then do 
        putStrLn "Goodbye!"
        return() 
        else do 
        printBoard board

defaultBoard :: Board
defaultBoard = makeBoard allPoints