module Utils where

import System.Random
import Control.Monad.State as S
import Control.Monad.Random
import Control.Lens

-- shuffle
pickAtRandOrder :: [a] -> IO [a]
pickAtRandOrder line = do
    g <- newStdGen
    return $ [line !! x | x <- take (length line -1) (evalRand (randomIndices $ length line -1) g)]

--shuffle but only play N notes
pickFromScale :: [a] -> [Int] -> [a]
pickFromScale line randIndex = [line !! x | x <- take (length line -1) (randIndex)]

-- shuffle and return n notes
pickNFromScale :: [a] -> Int -> IO [a]
pickNFromScale line n = do
    g <- newStdGen
    return $ [line !! x | x <- take n (evalRand (randomIndices $length line -1) g)]

{-}
pickNFromScale3 line n = do
    g <- newStdGen
    let l = [line !! x | x <- take n (evalRand (randomIndices $length line -1) g)]
    return l
-}

--lookup1 :: (Eq a) => a -> [(a,b)] -> Maybe b
--lookup1 _key [] = []]
lookup1 key ((x,y):xys)
    | key == x = over (element x) (succ) y
    | otherwise = lookup1 key xys

--Random Monad
createRandomList :: (MonadRandom m, Random a, Num a) => a -> Int -> m [a]
createRandomList x n = do
    ret <- getRandomRs (0, x)
    let a = take n ret
    return a

randomIndices :: RandomGen g => Int -> Rand g [Int]
randomIndices n = do
  --g <- newStdGen
  randomNumber <- getRandomRs (0, n)
  return $ randomNumber 

shuffle :: [a] -> IO [a]
shuffle x = if length x < 2 then return x else do
    i <- System.Random.randomRIO (0, length(x)-1)
    r <- shuffle (take i x ++ drop (i+1) x)
    return (x!!i : r)

randomDist :: (RandomGen g, Random a, Num a) => (a -> a) -> State g a
randomDist distInv = liftM distInv (S.state (randomR (0,1)))

--map pred [1,2,3,4] 
--over (elements (odd)) (succ) [1,2,3,4,5,6,7]
--over (elements (>3)) (succ) [1,2,3,4,5,6,7]
a = over (elements (>3)) (succ) [1,2,3,4,5,6,7]

--(element 3 .~ (succ)) [1,2,3,4,5]

--over (element 4) (succ) [1,2,3,4,5,6,7]
