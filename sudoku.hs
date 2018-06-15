{- |
  Daniel Jacob Behnke
  06/06/18
  Sudoku solver implementation in Haskell
-}

module Sudoku where

import Data.Char
import Data.List
import Data.Maybe
import Data.Set (isSubsetOf, fromList)


-- | Defines the Sudoku type as a list of lists of Maybe Ints
data Sudoku = Sudoku [[Maybe Int]]
  deriving (Show, Eq)

  
-- | Extracts the actual rows from a Sudoku type
rows::Sudoku-> [[Maybe Int]]
rows (Sudoku rs) = rs


-- | Generates an example sudoku
example :: Sudoku
example = Sudoku[ [Just 3, Just 6, Nothing, Nothing, Just 7, Just 1, Just 2, Nothing, Nothing], [Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing], [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing], [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8], [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9], [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing], [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing], [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing], [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]]


-- | Generates a Sudoku with all values 1
allOnesSudoku :: Sudoku
allOnesSudoku = Sudoku (replicate 9 (replicate 9 (Just(1))))


-- | Generates the all blank sudoku
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))


-- | Tests whether a list of lists type sudoku is actually a 9x9 grid of Maybe Ints
isSudoku::Sudoku->Bool
isSudoku sud
  | length(rows sud) == 9 = foldr (\x y -> (length x) == 9 && y) True (rows sud)
  | otherwise = False
   
   
-- | Tests if a given Sudoku has numbers in every "box"
isSolved::Sudoku->Bool
isSolved sud
  | isSudoku sud = foldr ((\z w -> (foldr (\x y -> (isJust x) && y) True z) && w)) True (rows sud)
  | otherwise = False

  
-- | Converts a Maybe Int to a String, either "z " for Ints or ". " for nothing
toString::Maybe Int -> String
toString Nothing = ". "
toString (Just a) = (show a) ++ " "


-- | Prints out a grid of a given Sudoku
printSudoku::Sudoku->  IO ()
printSudoku sud
  | isSudoku sud = putStrLn((concatMap (\xs -> foldr (++) "\n" (map toString xs)) (rows sud)))
  | otherwise = putStrLn("Exception placeholder") -- I don't know how to do exceptions in Haskell

  
{---readSudoku::FilePath -> IO Sudoku
readSudoku f = (readFile f)
 |  where
  inner h ls
    |hIsEOF = 
    |hGetChar-}