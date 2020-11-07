module Organism where

import qualified Data.Map as M
import GraphicalAnalysis
import System.Directory

class Organism a where
  genotype :: a -> String

chartPath :: String
chartPath = "./graphics/charts/"

countGeno :: Organism a => [a] -> M.Map String Int
countGeno =
  foldr
    (\o -> M.unionWith (+) (M.fromList [(genotype o, 1)]))
    M.empty

countGenotypes :: Organism a => [[a]] -> [[(String, Int)]]
countGenotypes = map (M.toList . countGeno)

createPieCharts :: Organism a => [[a]] -> String -> IO ()
createPieCharts orgs name = do
  let genos = countGenotypes orgs
  let path = chartPath ++ name ++ "_Pie/"
  createDirectoryIfMissing True path
  makePieCharts genos $ path ++ name
