module Stats where

import Environment
import Organism

type Tracking = [Int]

type Post = [[Int]]

data TrackingStats = TrackingStats
  { reproductions :: Tracking,
    movements :: Tracking,
    crossovers :: Tracking
  }
  deriving (Show)

emptyTracking :: TrackingStats
emptyTracking = TrackingStats [] [] []

data PostStats = PostStats
  { sizes :: Post,
    depths :: Post,
    nrParams :: Post,
    diversity :: Post,
    ages :: Post,
    resourceGrowth :: Tracking,
    popGrowth :: Tracking
  }
  deriving (Show)

type TrackingStatsCalc = Int -> TrackingStats -> TrackingStats

type ManipulateTrackingStats = TrackingStats -> TrackingStats

reproductionAddition :: TrackingStatsCalc
reproductionAddition i (TrackingStats r m c) = TrackingStats (r ++ [i]) m c

addReproduction :: ManipulateTrackingStats
addReproduction = reproductionAddition 1

addNoneReproduction :: ManipulateTrackingStats
addNoneReproduction = reproductionAddition 0

movementsAddition :: TrackingStatsCalc
movementsAddition i (TrackingStats r m c) = TrackingStats r (m ++ [i]) c

addMovement :: ManipulateTrackingStats
addMovement = movementsAddition 1

addNoneMovement :: ManipulateTrackingStats
addNoneMovement = movementsAddition 0

crossoverAddition :: TrackingStatsCalc
crossoverAddition i (TrackingStats r m c) = TrackingStats r m (c ++ [i])

addCrossover :: ManipulateTrackingStats
addCrossover = crossoverAddition 1

addNoneCrossover :: ManipulateTrackingStats
addNoneCrossover = crossoverAddition 0

calcMetric :: Organism a => (a -> Int) -> [Environment a] -> Post
calcMetric _ [] = []
calcMetric f (x : xs) = map f (getAllOrgs x) : calcMetric f xs

calcDualMetric :: Organism a => (a -> a -> Int) -> [Environment a] -> Post
calcDualMetric _ [] = []
calcDualMetric f (x : xs) = [f l r | l <- orgs, r <- orgs] : calcDualMetric f xs
  where
    orgs = getAllOrgs x

postCalculation ::
  Organism a =>
  [Environment a] ->
  (a -> Int) ->
  (a -> Int) ->
  (a -> Int) ->
  (a -> a -> Int) ->
  (a -> Int) ->
  PostStats
postCalculation env sizeCalc depthCalc paramCalc diversityCalc ageCalc =
  PostStats
    (calcMetric sizeCalc env)
    (calcMetric depthCalc env)
    (calcMetric paramCalc env)
    (calcDualMetric diversityCalc env)
    (calcMetric ageCalc env)
    (map countResources env)
    (map countOrgs env)
