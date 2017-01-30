{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}

import qualified Data.Map as M
import Data.List (sortBy)
import Data.Char
import Data.Ord  (Down (..), comparing)
import System.Random
import Control.Monad.Random 

type Randm a = Rand StdGen a

type Link a   = (a, Int)
data MChain a = Map a [Link a] deriving (Show)
type Words    = [String]

 
links :: Words -> [(String, [Link String] )]
links []        = []
links [x, y]    = [(x, [(y, 1)])]
links (w:w':ws) = (w, [(w', 1)]):links (w':ws)
links _         = []

merge :: [Link String] -> [Link String] -> [Link String]
merge ps ps' = condense $ ps ++ ps'

-- -- -- i.e. [("x", 1), ("y", 1) ("x",3)] -> [("x", 4), ("y", 1)] 
condense :: [Link String] -> [Link String]
condense = sortBy (comparing (Down . snd))  . M.toList . M.fromListWith (+) 


chainWords :: Words -> M.Map String [Link String]
chainWords = M.fromListWith merge . links 
--("so",[("much.",4),("nice.",3), ("sad",3), ("little", 2), ("dear", 1)])
select :: Int -> [Link String] -> String
select i  ((w, c) :ls)= selected i c ((w, c) :ls) where
    selected i ix ((w, c) :ls)
        | i <= ix = w
        | otherwise = selected i (ix + c) ls

randomN :: Int -> Randm Int
randomN n  = getRandomR (1, n)

upperBound :: [Link a] -> Int
upperBound =foldr (\x a -> a + snd x) 0 
-- upperBound ls =foldr (\x a -> a + snd x) 0 ls

main :: IO ()
main = do
  --input <- readFile "speeches.txt"
  let input = "Thank you so much.  That's so nice.  Isn't he a great guy.  He doesn't get a fair press;"
  print $ chainWords . map (filter (\x -> ord x > 32 && ord x < 126)) . words $ input
 
  

