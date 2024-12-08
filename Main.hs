module Main where
import Data.Char
import System.IO
import Data.Maybe
import Text.Read
import Morrissolver
import Control.Monad

data Phase = Set | Move | Fly deriving Eq

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
            placePhase Put (toPlayer playerCol) (toPlayer enemyCol) (toPlayer playerCol) 0 defaultBoard False


placePhase :: Action -> Player -> Player -> Player -> Int -> Board -> Bool -> IO ()
placePhase turn player enemy actor count board mill = do
    if mill then 
        if actor == player then enemyRemove board player enemy count Set else playerRemove board player enemy count Set
        else if count > 8 then playPhase turn player enemy actor board mill else
        if actor == player then do
            putStrLn "\nPlace your piece [ (x, y) ]..."
            printBoard board
            inputPlace <- getLine
            let input = map toUpper inputPlace
                placement = parseTuple inputPlace
                legal = legalMoves board
            if testInput input then applyInput input board
            else if (placement, O) `elem` legal then do
                newBoard <- placePiece board player placement
                placePhase turn player enemy enemy count newBoard mill
            else do
                putStrLn ("Cannot place a piece at " ++ show placement)
                placePhase turn player enemy player count board mill
        else do
            let legal = fst (head (legalMoves board))
            putStrLn ("\nEnemy placed a piece on " ++ show legal)
            newBoard <- placePiece board enemy legal
            placePhase turn player enemy player (count + 1) newBoard mill

enemyRemove :: Board -> Player -> Player -> Int -> Phase -> IO ()
enemyRemove board player enemy count phase = do 
    let remPoint = fst (head [(x, y) | (x, y) <- board, y == player])
        newBoard = makeMove board enemy remPoint Remove
    putStrLn ("Enemy removed your piece at " ++ show remPoint ++ "!")
    if phase == Set then placePhase Put player enemy player count board False else undefined

playerRemove :: Board -> Player -> Player -> Int -> Phase -> IO ()
playerRemove board player enemy count phase = do
    putStrLn "You got a mill! Choose an opponents piece to remove..."
    inputPlace <- getLine 
    let input = map toUpper inputPlace
        remPoint = parseTuple inputPlace
        pointPlayer = lookupPlayer board remPoint
    if pointPlayer == enemy then do
        let newBoard = makeMove board player remPoint Remove
        putStrLn ("You removed the enemy's piece at  " ++ show remPoint ++ "!")
        if phase == Set then placePhase Put player enemy enemy count board False else undefined
    else if pointPlayer == O then do 
        putStrLn "That space is empty! Pick a space that's occupied by the enemy!"
        playerRemove board player enemy count phase
        else do 
            putStrLn "It may not be the best idea to remove your own piece...\nPick one of the enemy's pieces!"
            playerRemove board player enemy count phase

playPhase :: Action -> Player -> Player -> Player -> Board -> Bool -> IO ()
playPhase turn player enemy actor board mill = do
    putStrLn "Beginning playPhase\n" 
    printBoard board

placePiece :: Board -> Player -> Point -> IO Board
placePiece board player placement = do
    let newBoard = makeMove board player placement Put
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