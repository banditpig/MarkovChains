
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}

import Data.List
import qualified Data.Map as M

data  MChain a = Map a [(String, Int)] deriving (Show)
type Words = [String]

updateOne :: String -> [(String, Int)] -> [(String, Int)]
updateOne s  = update (s, 1)

update :: (String, Int)  -> [(String, Int)] -> [(String, Int)]
update (s, n )  [] = [(s, n)]
update (s, n) (h@(s', n'):xs)
    | s == s' = (s', n' + n) : xs
    | otherwise = h : update (s, n) xs

merge :: [(String, Int)] -> [(String, Int)] -> [(String, Int)]
merge = foldr update


chainWords :: Words ->  MChain String
chainWords = undefined

main :: IO ()
main = do
    print ""