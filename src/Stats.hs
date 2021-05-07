{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Stats where

import Data.Aeson
import Environment
import GHC.Generics
import Organism

type Tracking = [Int]

type Post = [[Int]]

data TrackingStats = TrackingStats
  { reproductions :: Tracking,
    movements :: Tracking,
    crossovers :: Tracking,
    mutations :: Tracking
  }
  deriving (Show, Generic, ToJSON)

emptyTracking :: TrackingStats
emptyTracking = TrackingStats [] [] [] []

data PostStats = PostStats
  { sizes :: Post,
    depths :: Post,
    nrParams :: Post,
    diversity :: [[Double]],
    ages :: Post,
    resourceGrowth :: Tracking,
    popGrowth :: Tracking,
    ancestor1Diff :: [[Double]]
  }
  deriving (Show, Generic, ToJSON)

type TrackingStatsCalc = Int -> TrackingStats -> TrackingStats

type ManipulateTrackingStats = TrackingStats -> TrackingStats

reproductionAddition :: TrackingStatsCalc
reproductionAddition i (TrackingStats r m c mu) = TrackingStats (r ++ [i]) m c mu

addReproduction :: ManipulateTrackingStats
addReproduction = reproductionAddition 1

addNoneReproduction :: ManipulateTrackingStats
addNoneReproduction = reproductionAddition 0

movementsAddition :: TrackingStatsCalc
movementsAddition i (TrackingStats r m c mu) = TrackingStats r (m ++ [i]) c mu

addMovement :: ManipulateTrackingStats
addMovement = movementsAddition 1

addNoneMovement :: ManipulateTrackingStats
addNoneMovement = movementsAddition 0

crossoverAddition :: TrackingStatsCalc
crossoverAddition i (TrackingStats r m c mu) = TrackingStats r m (c ++ [i]) mu

addCrossover :: ManipulateTrackingStats
addCrossover = crossoverAddition 1

addNoneCrossover :: ManipulateTrackingStats
addNoneCrossover = crossoverAddition 0

mutationAddition :: TrackingStatsCalc
mutationAddition i (TrackingStats r m c mu) = TrackingStats r m c $ mu ++ [i]

addMutation :: ManipulateTrackingStats
addMutation = mutationAddition 1

addNoneMutation :: ManipulateTrackingStats
addNoneMutation = mutationAddition 0

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
  [[Double]] ->
  (a -> Int) ->
  [[Double]] ->
  PostStats
postCalculation env sizeCalc depthCalc paramCalc divers ageCalc =
  PostStats
    (calcMetric sizeCalc env)
    (calcMetric depthCalc env)
    (calcMetric paramCalc env)
    divers
    (calcMetric ageCalc env)
    (map countResources env)
    (map countOrgs env)
