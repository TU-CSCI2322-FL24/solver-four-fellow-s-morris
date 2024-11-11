module Main where
import Data.Char
import System.IO

game = True

main :: IO ()
main = do
    putStrLn "Select player color (B/W): "
    input <- getLine
    let playerCol = map toUpper input
    if playerCol /= "B" && playerCol /= "W" then do error "Invalid color" else putStrLn ("You selected " ++ playerCol ++ "\n")
    if game then do
        putStrLn ("Place your piece (x, y)... " ++ "\n")
        printBoard else putStrLn "."


printBoard :: IO()
printBoard = do
    file <- openFile "Board.txt" ReadMode
    board <- hGetContents file
    putStrLn board
    hClose file
    