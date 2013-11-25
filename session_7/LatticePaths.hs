import Data.List
import Control.Monad.Memo

data Moves = D | R

space' 0 = [[]]
space' x = [q:qs | qs <- space' (x-1), q <- ['D', 'R']]

space n = permutations $ (take n $ cycle "R") ++ (take n $ cycle "D")

latticePaths n = length $ nub $ space n

paths 0 0 = 0
paths 0 _ = 1
paths _ 0 = 1
paths r d = (paths (r - 1) d) + (paths r (d - 1))

--pathsm :: (Num n, Ord n, MonadMemo (n, n) n m) => n -> n -> m n
pathsm 0 0 = return 0
pathsm 0 _ = return 1
pathsm _ 0 = return 1
pathsm r d = do
  r1 <- for2 memo pathsm (r - 1) d
  d1 <- for2 memo pathsm r (d -1)
  return (r1 + d1)

evalPathsm :: (Num n, Ord n) => n -> n -> n
evalPathsm r d = startEvalMemo $ pathsm r d

runPathsm n = evalPathsm n n
--main = putStrLn $ show $ paths
