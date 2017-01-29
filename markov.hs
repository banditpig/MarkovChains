
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}

import Data.List
import qualified Data.Map as M

data  MChain a = Map a [(String, Int)] deriving (Show)
type Words = [String]

cw :: Words -> [(String, [(String, Int)] )]
cw [] = []
cw (x:y:[]) = [(x, [(y, 1)])]
cw (w:w':ws) = (w, [(w', 1)]):cw (w':ws)
cw _         = []

chainWords :: Words -> M.Map String [(String, Int)]
chainWords wds = M.fromListWith (f . (++)) (cw wds) where f = id
-- f will combine duplicate tuples ie [("x", 1), ("x",3)] -> [("x", 4)]

main :: IO ()
main = do
    print ""