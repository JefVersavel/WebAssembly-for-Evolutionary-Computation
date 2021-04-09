module Ancestor where

import AST
import Seeding
import SysCall
import Test.QuickCheck
import Test.QuickCheck.Random

seeder :: Int -> Int -> Int -> IO SysCall
seeder seed sze dpth =
  generate $
    useSeed gen $
      frequency
        [(sze, return Reproduction), (sze - dpth, elements [Up, Down, Rght, Lft]), (dpth, return None)]
  where
    gen = mkQCGen seed

type Seeding = (Int, Int, Int)

showVerticalList :: Show a => [a] -> IO ()
showVerticalList [] = putStrLn ""
showVerticalList (x : xs) = do
  print x
  showVerticalList xs

redirectIO :: (IO a, b) -> IO (a, b)
redirectIO (l, r) = do
  lft <- l
  return (lft, r)

checkForReproduction :: [Int] -> IO [Seeding]
checkForReproduction seeds = do
  let sizes = [15 .. 20]
  let depths = [5 .. 15]
  syscallsSeedings <-
    sequence
      [ redirectIO (seeder seed sze dpth, (seed, sze, dpth))
        | sze <- sizes,
          dpth <- depths,
          sze > 2 * dpth,
          seed <- seeds
      ]
  return $ map snd $ filter (isReproduction . fst) syscallsSeedings

isReproduction :: SysCall -> Bool
isReproduction Reproduction = True
isReproduction _ = False

lookForSeeds :: IO ()
lookForSeeds = do
  let seeds = [0 .. 10]
  reproductions <- checkForReproduction seeds
  showVerticalList reproductions
