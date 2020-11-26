module Resource where

import Environment
import Seeding
import Test.QuickCheck
import Test.QuickCheck.Random

-- This module contains some functions to handle the resources
-- and define some rules for the resources in the environment.

-- | Returns the amount of total resources there are in the environment
-- relative to the amount of cells in the environment.
totalResources :: Double
totalResources = 1

-- | Returns whether the organism places its outcome into one of the neighbouring cells.
-- When False the outcome is placed on the cell of the orgamism itself.
neighbourDistribution :: Bool
neighbourDistribution = True

-- | Randomly picks one of the positions from a list.
distributeResource :: QCGen -> [Pos] -> IO Pos
distributeResource gen positions = generate $ useSeed gen $ elements positions
