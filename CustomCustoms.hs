module CustomCustoms
    (customCustoms)
where

import Data.List (sort, nub)
-- https://adventofcode.com/2020/day/6

mylines :: [String] -> [String]
mylines [] = []
mylines ("":xs) = mylines xs
mylines xs =
    let (line, rest) = span (/= "") xs
    in unwords line: mylines rest

customCustoms::IO()
customCustoms = do
    contents <- readFile "customCustoms.txt"
    let answers = sum $ map (length . (sort . nub . filter (/= ' ')) ) $ mylines $ lines contents
    print answers
