{-# LANGUAGE BlockArguments #-}
module PlaneSeating
  ( execute
  , testfunc
  , testfunc2)
  where
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Control.Monad.ST (runST, ST)
import Control.Monad (forM_)
-- https://adventofcode.com/2020/day/11
-- Double Buffering implementation.



execute ::(Int, Int) -> U.Vector Char -> Int
execute k xse = runST $ do
    let len = U.length xse
    bufA <- U.thaw xse
    bufB <- MU.new len :: ST s (MU.MVector s Char)
    let     loop curr dest = do
                forM_ [0..len - 1] $ \idx -> do
                    currentCell <- MU.unsafeRead curr idx
                    neighbors <- getneighbors k idx curr
                    let newValue = updateValueTo currentCell neighbors
                    MU.unsafeWrite dest idx newValue
                -- indentation
                --
                identical <- isIdentical (len - 1) curr dest
                if identical then do
                    finalVec <- U.freeze dest
                    return $ U.length $ U.filter (== 'O') finalVec
                else
                    loop dest curr
    loop bufA bufB



--- Helper functions

isIdentical :: Int -> MU.MVector s Char -> MU.MVector s Char -> ST s Bool
isIdentical len xse vse = go 0
  where
    go i = i `seq` do
      if i >= len
         then return True
         else do
           a <- MU.unsafeRead xse i
           b <- MU.unsafeRead vse i
           if a == b
              then go (i + 1)
              else return False



--- L -> empty
--- O -> occupied
--- . -> floor
occupancyCount :: [Char] -> Int
occupancyCount xse = length $ filter (== 'O') xse


updateValueTo :: Char -> [Char]-> Char
updateValueTo '.' _ = '.'
updateValueTo curr neighbors'
    | curr == 'L' && n == 0 = 'O' -- Empty seat becomes occupied
    | curr == 'O' && n >= 4 = 'L' -- Occupied seat becomes empty
    | otherwise             = curr -- No change otherwise
    where n = length $ filter (== 'O') neighbors'


getneighbors::(Int, Int) -> Int -> MU.MVector s Char -> ST s [Char]
getneighbors k idx xse = do
    let idxs = getneighborsHelper k idx
    mapM (\x -> MU.unsafeRead xse x ) idxs


getneighborsHelper::(Int, Int) -> Int -> [Int]
getneighborsHelper (width, height) idx =
  let (rw, cl) = idx `divMod` width
      neighbors = [(x, y) | x <- [-1..1], y <- [-1..1], (x, y) /= (0,0)]
      in
        [ (drw * width) + dcl |
          (dr, dc) <- neighbors
          , let drw = dr + rw
          , let dcl = dc + cl
          , drw >=0 && drw < height
          , dcl >=0 && dcl < width
        ]

testfunc ::IO()
testfunc = do
    let xse = U.fromList "L.LLLLLLL"
    let value = execute (3,3) xse
    print value


-- | Reads the file, calculates dimensions, and returns (k, vector)
loadGrid :: FilePath -> IO ((Int, Int), U.Vector Char)
loadGrid path = do
    contents <- readFile path
    let     rows = lines contents
            numRows = length rows
            numCols = length (head rows)
            -- Flatten the list of strings into a single string without newlines
            flatStr = concat rows
    return ((numRows, numCols), U.fromList flatStr)

testfunc2::IO()
testfunc2 = do
    -- 1. Load the data
    (k, grid) <- loadGrid "planeseating.txt"

    -- 2. Run your ST simulation
    let result = execute k grid

    -- 3. Print the answer
    putStrLn $ "Final occupied seats: " ++ show result


