module Resource where

import qualified Data.List.Ordered as List

-- This module contains some functions to handle the resources
-- and define some rules for the resources in the environment.

-- | Returns the amount of total resources there are in the environment
-- relative to the amount of cells in the environment.
totalDoubles :: Double
totalDoubles = 1

-- | Returns whether the organism places its outcome into one of the neighbouring cells.
-- When False the outcome is placed on the cell of the orgamism itself.
-- the organism will only distribute its register if it also took a resource
neighbourDistribution :: Bool
neighbourDistribution = True

-- | Function that returns true if two doubles match.
-- This may be changed to experiment.
(~=) :: Double -> Double -> Bool
f ~= s = minus < 5 || - minus < 5
  where
    minus = f - s

-- | Returns the double in the list that is closest to the given double.
getClosestDouble :: [Double] -> Double -> Double
getClosestDouble list res = head $ List.sort $ map (abs . (res -)) list

-- | Returns true if the given double matches on one of the doubles in the list.
-- Returns false otherwise.
matches :: [Double] -> Double -> Bool
matches resources d = getClosestDouble resources d ~= d

-- | Returns the index of the first match in the given list of doubles.
-- When no match is found then Nothing is returned.
getMatch :: [Double] -> Double -> Maybe Int
getMatch [] _ = Nothing
getMatch (x : xs) r
  | x ~= r = Just 0
  | otherwise = getMatch xs r >>= \i -> Just (1 + i)
