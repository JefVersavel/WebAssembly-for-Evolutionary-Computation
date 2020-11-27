module Double where

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

matchDouble :: Double -> Double -> Bool
matchDouble res reg = res ~= reg

(~=) :: Double -> Double -> Bool
f ~= s = minus < 5 || - minus < 5
  where
    minus = f - s

getClosestDouble :: [Double] -> Double -> Double
getClosestDouble list res = head $ List.sort $ map (abs . (res -)) list

matches :: [Double] -> Double -> Bool
matches resources d = matchDouble (getClosestDouble resources d) d
