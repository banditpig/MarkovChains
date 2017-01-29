{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}

import qualified Data.Map as M
import Data.List (sortOn)
import Data.Ord  (Down (..))

data MChain a = Map a [(a, Int)] deriving (Show)
type Words = [String]

links :: Words -> [(String, [(String, Int)] )]
links []        = []
links (x:y:[])  = [(x, [(y, 1)])]
links (w:w':ws) = (w, [(w', 1)]):links (w':ws)
links _         = []

merge :: [(String, Int)] -> [(String, Int)] -> [(String, Int)]
merge ps ps' = condense $ ps ++ ps'

-- -- -- i.e. [("x", 1), ("y", 1) ("x",3)] -> [("x", 4), ("y", 1)] 
condense :: [(String, Int)] -> [(String, Int)]
condense = sortOn Down . M.toList . M.fromListWith (+) 

chainWords :: Words -> M.Map String [(String, Int)]
chainWords = M.fromListWith merge . links 

main :: IO ()
main = print ""