
import Data.List
import Data.List.Split as Split
import Text.Regex
import Data.Maybe

type Board = [[Char]]

newBoard :: Board
newBoard = [[' ',' ',' '],[' ',' ',' '],[' ',' ',' ']]

showBoard :: Board -> IO()
showBoard board = do putStrLn ""
                     mapM_ putStrLn $ intersperse "-----" $ map (\r -> intersperse '|' r) board 
                     putStrLn ""

val :: Board -> Int -> Int -> Char
val board row col = board !! row !! col

sym :: Int -> Char
sym player
  | player == 1 = 'x'
  | player == 2 = 'o'

otherPlayer :: Int -> Int
otherPlayer player
  | player == 1 = 2
  | player == 2 = 1

isValidMovement :: Board -> Int -> Int -> Bool
isValidMovement board row col = (val board row col) == ' '

doMovement :: Board -> Int -> Int -> Int -> Maybe Board
doMovement board row col player 
  | isValidMovement board row col = Just $ (take row board) ++ [(substitute (board !! row) col player)] ++ drop (row + 1) board
  | otherwise                     = Nothing
    where substitute row col player = (take col row) ++ [sym player] ++ drop (col + 1) row

isWinner :: Board -> Int -> Bool
isWinner board player              = winByRow  board player || 
                                     winByCol  board player ||
                                     winByDiag board player
  where winByRow  board player     = anyRowEqualTo board             player
        winByCol  board player     = anyRowEqualTo (transpose board) player
        winByDiag board player     = anyRowEqualTo diagLines         player
        anyRowEqualTo board player = any (\row -> all (== sym player) row) board
        transpose board            = (zipWith3 (\x y z -> [x,y,z]) (board !! 0) (board !! 1) (board !! 2))
        diagLines                  = [[val board 0 0, val board 1 1, val board 2 2], 
                                      [val board 0 2, val board 1 1, val board 2 0]]

readMovement :: Int -> (String,String) -> IO (Int, Int)
readMovement player names = do putStrLn $ "\nYour turn " ++ nameOf player ++ ":"
                               line <- getLine
                               if correctInput line
                               then do let (row:col:[]) = Split.splitOn "," line
                                       return ((read row ::Int)-1, (read col :: Int)-1)
                               else do putStrLn "\nWrong movement! Try again."
                                       readMovement player names
  where correctInput input = isJust $ matchRegex (mkRegex "^[123],[123]$") input
        nameOf player      = if player == 1 then fst names else snd names

play :: Board -> (String,String) -> Int -> IO (Int)
play board names player = do showBoard board
                             movement <- readMovement player names
                             let updatedBoard = doMovement board (fst movement) (snd movement) player
                             case updatedBoard of
                               Just board -> if isWinner board player 
                                             then do showBoard board
                                                     return (player)
                                             else play board names $ otherPlayer player
                               Nothing    -> do putStrLn "\nThat position is already used! Try again."
                                                play board names player

main :: IO ()
main = do putStrLn "Name of the first player: " 
          p1Name <- getLine
          putStrLn "Name of the second player: " 
          p2Name <- getLine
          result <- play newBoard (p1Name, p2Name) 1 
          case result of
            0 -> putStrLn "\nIt's a draw ... come on! It's just tic-tac-toe!"
            1 -> putStrLn $ "\nYou won " ++ p1Name ++ " !!!"
            2 -> putStrLn $ "\nYou won " ++ p2Name ++ " !!!"
