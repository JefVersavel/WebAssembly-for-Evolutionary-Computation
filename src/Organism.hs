module Organism where

import qualified Data.Map as M
import GraphicalAnalysis
import System.Directory

-- | Class representing organisms with a genotype that uniquely identifies a unique creature.
class (Show a, Eq a) => Organism a where
  genotype :: a -> String

-- | The path at which the generated charts are located.
chartPath :: String
chartPath = "./graphics/charts/"

-- | Generates a map with the number of organisms of each genotype from the given list of organisms.
countGeno :: Organism a => [a] -> M.Map String Int
countGeno =
  foldr
    (\o -> M.unionWith (+) (M.fromList [(genotype o, 1)]))
    M.empty

countGenotypes :: Organism a => [[a]] -> [[(String, Int)]]
countGenotypes = map (M.toList . countGeno)

-- | Create a pie chart with the population distribution.
createPieCharts :: Organism a => [[a]] -> String -> IO ()
createPieCharts orgs name = do
  let genos = countGenotypes orgs
  let path = chartPath ++ name ++ "_Pie/"
  createDirectoryIfMissing True path
  makePieCharts genos $ path ++ name
