module Main where
import Data.Char
import System.IO
import Data.Maybe
import Text.Read
import Morrissolver
import Control.Monad ()
import System.Console.GetOpt
import System.Environment

data Phase = Set | Move | Fly deriving Eq
-- I am adding this for the flags 

--why can i not add ( maybe Int) to Depth
data Flag = Winner | Depth String | Help | MoveF String deriving (Show, Eq)


options :: [OptDescr Flag]
options=[ Option ['w'] ["winner"] (NoArg Winner) "Print out the best move, using the exhaustive search"
        , Option ['d'] ["depth"] (ReqArg Depth "<num>") "Specify <num> as a cutoff depth, instead of our default"
        , Option ['h'] ["help"] (NoArg Help) "Print out a good help function and quit the program"
        , Option ['m'] ["move"] (ReqArg MoveF "<move>") "Should apply <move> and print out the resulting board"

        ]



main :: IO ()
main = do
    args <- getArgs
    let (flags, inputs, errors) = getOpt Permute options args
    if Help `elem` flags || not (null errors) 
        then putStrLn $ usageInfo "Morris [options] filename" options
        else do 
            when (Winner `elem` flags && any depthFlagPresent flags) $
                putStrLn "Warning: The -d flag has no effect when combined with the -w flag."
            let depth = handleDepthFlag flags
            if Winner `elem` flags
                then do
                    let bestMAction = bestMove initialGame --Replace with game state? Do I need to read a seperate file in? 
                    putStrLn $ "Best move: " ++ show bestMAction
                else do 
                    putStrLn $ "Selected depth: " ++ show depth

-- checking for depth flag 
depthFlagPresent :: Flag -> Bool
depthFlagPresent (Depth _) = True
depthFlagPresent _ = False 

-- default depth
defaultDepth :: Int
defaultDepth = 4 

-- handles Depth Flag
-- default is 4 
handleDepthFlag flags =
    case [dep | Depth dep <- flags] of
        []        -> defaultDepth 
        [Nothing] -> defaultDepth 
        [Just dep] | dep > 0 -> dep   
        [Just dep]  -> error "Error: Depth must be a positive integer."
        _         -> error "Error: Multiple depth values provided."
--handleDepthFlag :: [Flag] -> Maybe Int
--handleDepthFlag [] = 4
--handleDepthFlag (Depth d:xs) = read d
--handleDepthFlag (x:xs) = handleDepthFlag xs


-- Winner need to use bestMove and WhoWillWin 
-- input and output for the exhaustive search should be an Action and a Game 

--handles the Move Flag
isThereMove :: [Flag] -> Bool
isThereMove (MoveF _) = True
isThereMove _ = False

--Part of the orginal main before 
  --do args <- getArgs
     --let (flags, inputs, errors) = getOpt Permute options args
     --if Help `elem` flags || not (null errors) 
     --then putStrLn $ usageInfo "Morris [options] [filename]" options
     --else 
      --do let fname = if null inputs then "Board.txt" else head inputs
             --contents <- readFile fname


-- Now have to do the winner flag 

placePhase :: Game -> Player -> IO ()
placePhase game player = if getMill game then enemyRemove game (opponent player) else do
    i <- getLine
    let input = map toUpper i
    if input `elem` quitInputs then quitGame else if input `elem` showBoardInputs then do
        showBoard game
        placePhase game player
    else do
        let placement = parseTuple input
            legal = [fst l | l <- legalPlaces (getBoard game)]
        if placement `elem` legal then do
            let g = makeMove game (Put placement)
            putStrLn ("Placed a piece at " ++ show placement ++ "\n\n")
            enemyPlace g (opponent player)
        else do
            putStrLn "Invalid move. Did you format your input as (x,y)?\nPlace a piece!\n"
            placePhase game player

playerRemove :: Game -> Player -> IO ()
playerRemove game player = do
    putStrLn ("You got a mill! Select one of " ++ playerString (Just (opponent player)) ++ "'s pieces to remove!")
    i <- getLine
    let input = map toUpper i
    if input `elem` quitInputs then quitGame else if input `elem` showBoardInputs then do
        showBoard game
        playerRemove game player
    else do
        let placement = parseTuple input
            legal = [fst l | l <- getPlayerPlaces game (Just (opponent player))]
        if placement `elem` legal then do
            let g = makeMove game (Remove placement)
            putStrLn ("Removed piece at " ++ show placement ++ "\n\n")
            enemyPlace g (opponent player)
        else do
            putStrLn "Invalid move. Did you format your input as (x,y)?\nRemove a piece!\n"
            playerRemove game player

enemyPlace :: Game -> Player -> IO ()
enemyPlace game enemy = if getMill game then playerRemove game (opponent enemy) else do 
    let legal = head [fst l | l <- legalPlaces (getBoard game)]
        g = makeMove game (Put legal)
    putStrLn ("Player " ++ playerString (Just enemy) ++ " placed a piece at " ++ show legal ++ "\n\nYour turn!\n")
    placePhase g (opponent enemy)

enemyRemove :: Game -> Player -> IO ()
enemyRemove game enemy = do
    let plyPieces = head [fst l | l <- getPlayerPlaces game (Just (opponent enemy))]
        g = makeMove game (Remove plyPieces)
    putStrLn ("Player " ++ playerString (Just enemy) ++ " removed your piece at " ++ show plyPieces ++ "\n\nYour turn!\n")
    placePhase g (opponent enemy)

quitInputs :: [String]
quitInputs = ["QUIT", "Q", "EXIT", "END"]

showBoardInputs :: [String]
showBoardInputs = ["BOARD", "STATE", "GAME", "G", "SHOW"]

showBoard :: Game -> IO ()
showBoard game = putStrLn ("\n" ++ prettyPrint game)

quitGame :: IO ()
quitGame = putStrLn "Goodbye"

parseTuple :: String -> Point
parseTuple s =
    let trimmed = filter (`notElem` " ()") s
    in case break (== ',') trimmed of
        (aStr, ',' : bStr) ->
            case (readMaybe aStr, readMaybe bStr) of
                (Just intA, Just intB) -> (intA, intB)
                _ -> (99999, 99999)
        _ -> (99999, 99999)

{-

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
defaultBoard = makeBoard allPoints -}