module Utils where

import System.Random
import Control.Monad.State as S
import Control.Monad.Random
import Control.Lens
import Data.Bool


-- Enum Cycling. Useful to cycle data Key which derives Enum
next :: (Eq a, Enum a, Bounded a) => a -> a
next = bool minBound <$> succ <*> (/= maxBound)

prev :: (Eq a, Enum a, Bounded a) => a -> a
prev = bool maxBound <$> pred <*> (/= minBound)

--helper functon to avoid exceeding maximum beats in bar
listSumCeiling :: (Ord a, Num a) => [a] -> a -> [a]
listSumCeiling list n = takeWhile (< n) (scanl1 (+) list)

-- creates Random list such that sum is under certain number
--write function such that sum is == n?. ah well.
pickNSumCeiling :: [a] -> Int -> IO [a]
pickNSumCeiling line n = do
    g <- newStdGen
    return $ [line !! x | x <- listSumCeiling (evalRand (randomIndices $length line -1) g) 4]

-- return n random elements from list
pickNRandom :: [a] -> Int -> IO [a]
pickNRandom line n = do
    g <- newStdGen
    return $ [line !! x | x <- take n (evalRand (randomIndices $length line -1) g)]

randomIndices :: RandomGen g => Int -> Rand g [Int]
randomIndices n = do
  randomNumber <- getRandomRs (0, n)
  return $ randomNumber 

-- useful for 12 tone music
shuffle :: [a] -> IO [a]
shuffle x = if length x < 2 then return x else do
    i <- System.Random.randomRIO (0, length(x)-1)
    r <- shuffle (take i x ++ drop (i+1) x)
    return (x!!i : r)

--function to replace element in list
replace :: Traversable t => t a -> (Int, a) -> t a
replace list (index,el) = (element index .~ el ) list

--------------------------TESTS-------------------------------------------
--Random Monad
createRandomList :: (MonadRandom m, Random a, Num a) => a -> Int -> m [a]
createRandomList x n = do
    ret <- getRandomRs (0, x)
    return $ take n ret

randomDist :: (RandomGen g, Random a, Num a) => (a -> a) -> State g a
randomDist distInv = liftM distInv (S.state (randomR (0,1)))

--------------------------------------------------------------------------
