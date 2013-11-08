import Data.List

positions sol = zip sol [1..8]

diag1 :: (Int,Int) -> [(Int,Int)]
diag1 (r,c) = [(r',c') | r' <- [r-1,r-2..1], c' <- [c-1,c-2..1], r' >= 1, c' >= 1, r' - c' == r - c ]

diag2 :: (Int,Int) -> [(Int,Int)]
diag2 (r,c) = [(r',c') | r' <- [r+1..8], c' <- [c+1..8], r' <= 8, c' <= 8, r' - c' == r - c ]

diag3 :: (Int,Int) -> [(Int,Int)]
diag3 (r,c) = [(r',c') | r' <- [r+1..8], c' <- [c-1,c-2..1], r' <= 8, c' >= 1, r' + c' == r + c ]

diag4 :: (Int,Int) -> [(Int,Int)]
diag4 (r,c) = [(r',c') | r' <- [r-1,r-2..1], c' <- [c+1..8], r' >= 1, c' <= 8, r' + c' == r + c ]

diags pos =  diag1 pos ++ diag2 pos ++ diag3 pos ++ diag4 pos

isValid sol = all (\pos -> null $ intersect (positions sol) (diags pos)) (positions sol)

eightQueens = filter isValid $ permutations [1,2,3,4,5,6,7,8]