
import Data.List.Split as S

type Board = [[Int]]

newBoard :: Board
newBoard = [[0,0,0],[0,0,0],[0,0,0]]

value :: Board -> Int -> Int -> Int
value board row column = board !! row !! column

isValidMovement :: Board -> Int -> Int -> Bool
isValidMovement board row column = value board row column  == 0 

setMovement :: Board -> Int -> Int -> Int -> Board
setMovement board row column player = (take row board) ++ [(substitute (board !! row) column player)] ++ drop (row + 1) board
                               where substitute row column player = (take column row) ++ [player] ++ drop (column + 1) row

winByRow :: Board -> Int -> Bool
winByRow board player  = any (\row -> all (== player) row) board

winByColumn :: Board -> Int -> Bool
winByColumn board player  = any (\row -> all (== player) row) (zipWith3 (\x y z -> [x,y,z]) (board !! 0) (board !! 1) (board !! 2))

winByDiag :: Board -> Int -> Bool
winByDiag board player = any (\row -> all (== player) row) [[value board 0 0, value board 1 1, value board 2 2], [value board 0 2, value board 1 1, value board 2 0]]

isWinner :: Board -> Int -> Bool
isWinner board player = winByRow board player || winByColumn board player || winByDiag board player

getMovement :: IO (Int, Int)
getMovement = do line <- getLine
                 let (row:col:[]) = S.splitOn " " line
                 return (read row ::Int, read col :: Int)

nuevaFuncion uno dos =  uno + dos 

--play :: IO ()
--play player = do movement <- getMovement
--                 putStrLn "movement for " ++ show player
--                 play (if player == 1 then 2 else 1) 

--main :: IO ()
--main = do let board = newBoard
--          play 1


