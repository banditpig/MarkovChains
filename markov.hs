{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS
  -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}

import Control.Concurrent (threadDelay)
import Control.Monad.Random
import Data.Char
import Data.List (sortBy, isInfixOf)
import qualified Data.Map as M
import Data.Ord (Down(..), comparing)
import System.Random

type Randm a = Rand StdGen a

type Link a = (a, Int)

data MChain a =
  Map a
      [Link a]
  deriving (Show)

type Words = [String]

links :: Words -> [(String, [Link String])]
links [] = []
links [x, y] = [(x, [(y, 1)])]
links (w:w':ws) = (w, [(w', 1)]) : links (w' : ws)
links _ = []

merge :: [Link String] -> [Link String] -> [Link String]
merge ps ps' = condense $ ps ++ ps'

-- -- -- i.e. [("x", 1), ("y", 1) ("x",3)] -> [("x", 4), ("y", 1)] 
condense :: [Link String] -> [Link String]
condense = sortBy (comparing (Down . snd)) . M.toList . M.fromListWith (+)

chainWords :: Words -> M.Map String [Link String]
chainWords = M.fromListWith merge . links

select :: Int -> [Link String] -> String
select _ [] = "" -- won't happen... :)
select i ((w, n):ls)
  | i <= n = w
  | otherwise = select (i - n) ls

randomN :: Int -> Randm Int
randomN n = getRandomR (1, n)

upperBound :: [Link a] -> Int
upperBound = foldr (\x a -> a + snd x) 0

randomWord :: [Link String] -> IO String
randomWord lnks = do
  x <- randomRIO (1, upperBound lnks)
  return $ select x lnks

randomSentences :: String -> M.Map String [Link String] -> IO String
randomSentences seed mchain = randomSentence' seed 0 seed mchain
  where
    randomSentence' :: String
                    -> Int
                    -> String
                    -> M.Map String [Link String]
                    -> IO String
    randomSentence' rslt len sd chn
      | len >= 10 && hasFullStop sd = return rslt
        -- | length rslt > 130 = return rslt -- tweet
      | otherwise = do
        let lnks = (M.!) mchain sd
        newWd <- randomWord lnks
        randomSentence' (rslt ++ " " ++ newWd) (len + 1) newWd chn
      where
        hasFullStop = isInfixOf "."

main :: IO ()
main = do
  input <- readFile "speeches.txt"
  -- let input = "Thank you so much.  That's so nice.  Isn't he a great guy.  He doesn't get a fair press;"
  let mchain =
        chainWords .
        map (filter (\x -> ord x > 32 && ord x < 126 && ord x /= 34)) . words $
        input
  forever $ do
    let startWds = M.keys mchain
    sx <- randomRIO (1, length startWds)
    sen1 <- randomSentences ((!!) startWds sx) mchain
    print sen1
    putStrLn ""
    threadDelay 1000000
