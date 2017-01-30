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

select :: Int -> [Link String] -> String
select _ [] = "" -- won't happen... :)
select i ((w, n ):ls)
    | i <= n = w
    | otherwise = select (i - n) ls

randomN :: Int -> Randm Int
randomN n  = getRandomR (1, n)

upperBound :: [Link a] -> Int
upperBound =foldr (\x a -> a + snd x) 0 

randomWord :: [Link String] -> IO String
randomWord lnks = do
    x <- randomRIO (1, upperBound lnks)
    return $ select x lnks

randomSentence :: String -> M.Map String [Link String] -> IO String
randomSentence seed mchain = randomSentence' seed 0 seed mchain where
      randomSentence' :: String -> Int -> String -> M.Map String [Link String] -> IO String
      randomSentence' rslt len sd chn 
        | len == 10 = do return rslt
        | otherwise = do 
            let lnks =  (M.!)  mchain sd
            newWd <- randomWord lnks
            randomSentence' (f rslt newWd) (len+1) newWd chn where
               f s1 s2 = s1 ++ " " ++ s2



 -- sample2 freqs = do
 --     idx <- randomRIO (0, sum (map snd freqs) - 1)
 --    return $ indexFreqs idx freqs


main :: IO ()
main = do
  input <- readFile "speeches.txt"
  -- let input = "Thank you so much.  That's so nice.  Isn't he a great guy.  He doesn't get a fair press;"
  let mchain = chainWords . map (filter (\x -> ord x > 32 && ord x < 126)) . words $ input
  sen1 <- randomSentence "Thank" mchain
  sen2 <- randomSentence "Thank" mchain
  sen3 <- randomSentence "Thank" mchain


  print sen1
  print sen2
  print sen3

 
  

