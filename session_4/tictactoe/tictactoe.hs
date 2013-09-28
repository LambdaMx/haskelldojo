
import Data.List
import Data.List.Split as Split
import Text.Regex
import Data.Maybe

type Board = [[Char]]

newBoard :: Board
newBoard = [[' ',' ',' '],[' ',' ',' '],[' ',' ',' ']]

showBoard :: Board -> IO()
showBoard board = mapM_ putStrLn $ intersperse "-----" $ map (\r -> intersperse '|' r) board 

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
isWinner board player           = winByRow  board player || 
                                  winByCol  board player ||
                                  winByDiag board player
  where winByRow  board player  = anyRowEqualTo board             player
        winByCol  board player  = anyRowEqualTo (transpose board) player
        winByDiag board player  = anyRowEqualTo diagLines         player
        anyRowEqualTo board player = any (\row -> all (== sym player) row) board
        transpose board            = (zipWith3 (\x y z -> [x,y,z]) (board !! 0) (board !! 1) (board !! 2))
        diagLines                  = [[val board 0 0, val board 1 1, val board 2 2], 
                                      [val board 0 2, val board 1 1, val board 2 0]]

getMovement :: Int -> IO (Int, Int)
getMovement player = do putStrLn $ "Your turn player " ++ show player ++ ":"
                        line <- getLine
                        if correctInput line
                        then do let (row:col:[]) = Split.splitOn "," line
                                return (read row ::Int, read col :: Int)
                        else do putStrLn "Wrong movement! Try again:"
                                getMovement player
  where correctInput input = isJust $ matchRegex (mkRegex "^[012],[012]$") input

play :: Board -> Int -> IO ()
play board player = do showBoard board
                       movement <- getMovement player
                       let updatedBoard = doMovement board (fst movement) (snd movement) player
                       case updatedBoard of
                         Just board -> if isWinner board player 
                                       then do showBoard board
                                               putStrLn $ "You won player " ++ show player ++ " !!!"
                                               return ()
                                       else play board $ otherPlayer player
                         Nothing    -> do putStrLn "Wrong movement. Try again !!!"
                                          play board player
main :: IO ()
main = do play newBoard 1 
