import Data.List

isValidSolution sol   = all isValid fullPositions
  where fullPositions = zip sol [1..8]
        isValid pos   = null $ intersect fullPositions (diags pos)
        diags pos     = diag1 pos ++ diag2 pos ++ diag3 pos ++ diag4 pos
        diag1 (r,c)   = [(r',c') | r' <- [r-1,r-2..1], r' >= 1, c' <- [c-1,c-2..1], c' >= 1, r'-c' == r-c ]
        diag2 (r,c)   = [(r',c') | r' <- [r+1..8],     r' <= 8, c' <- [c+1..8],     c' <= 8, r'-c' == r-c ]
        diag3 (r,c)   = [(r',c') | r' <- [r+1..8],     r' <= 8, c' <- [c-1,c-2..1], c' >= 1, r'+c' == r+c ]
        diag4 (r,c)   = [(r',c') | r' <- [r-1,r-2..1], r' >= 1, c' <- [c+1..8],     c' <= 8, r'+c' == r+c ]

eightQueens = filter isValidSolution $ permutations [1,2,3,4,5,6,7,8]
