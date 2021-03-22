module Stats where

type Tracking = [Int]

type Post = [Int]

data TrackingStats = TrackingStats
  { reproductions :: Tracking,
    movements :: Tracking,
    crossovers :: Tracking,
    resourceGrowth :: Tracking,
    popGrowth :: Tracking
  }

data PostStats = PostStats
  { sizes :: Post,
    depths :: Post,
    nrParams :: Post,
    diversity :: Post,
    ages :: Post
  }

type TrackingStatsCalc = Int -> TrackingStats -> TrackingStats

type ManipulateTrackingStats = TrackingStats -> TrackingStats

reproductionAddition :: TrackingStatsCalc
reproductionAddition i (TrackingStats r m c res pop) = TrackingStats (r ++ [i]) m c res pop

addReproduction :: ManipulateTrackingStats
addReproduction = reproductionAddition 1

addNoneReproduction :: ManipulateTrackingStats
addNoneReproduction = reproductionAddition 0

movementsAddition :: TrackingStatsCalc
movementsAddition i (TrackingStats r m c res pop) = TrackingStats r (m ++ [i]) c res pop

addMovement :: ManipulateTrackingStats
addMovement = movementsAddition 1

addNoneMovement :: ManipulateTrackingStats
addNoneMovement = movementsAddition 0
