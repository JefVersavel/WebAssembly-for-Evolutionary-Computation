module ALife_1 where

import           Generators
import           WasmGenerator
import           ExecuteWasm
import           AST
import           Test.QuickCheck.Random
import           Test.QuickCheck.Gen
import           System.Random
import           GeneticOperations

type Organism = (ASTExpression, Double)

generateInitPop :: QCGen -> Int -> Double -> Int -> IO [Organism]
generateInitPop gen d ratio n = do
  expr <- sequence [ generate g | g <- rampedHalfNHalf gen d 1 ratio n ]
  return [ (e, 8) | e <- expr ]

getRegisters :: [Organism] -> [Double]
getRegisters orgs = [ i | (_, i) <- orgs ]

getExpressions :: [Organism] -> [ASTExpression]
getExpressions orgs = [ e | (e, _) <- orgs ]

executeOrganisms :: [Organism] -> IO [Organism]
executeOrganisms orgs = do
  let params = getRegisters orgs
  let expr   = getExpressions orgs
  serialized <- serializeExpressions expr params
  outcomes   <- sequence [ executeModule bs | (_, bs) <- serialized ]
  return $ zip expr outcomes

mutateOrganisms :: QCGen -> [Organism] -> Double -> IO [Organism]
mutateOrganisms _   orgs 0     = return orgs
mutateOrganisms gen orgs ratio = do
  let n       = round $ ratio * fromIntegral (length orgs)
  let nextGen = fst $ split gen
  let indexes = take n $ randomRs (0, length orgs - 1) nextGen
  mutateOrgList gen orgs 0 indexes

mutateOrgList :: QCGen -> [Organism] -> Int -> [Int] -> IO [Organism]
mutateOrgList _ orgs _ [] = return orgs
mutateOrgList _ []   _ _  = return []
mutateOrgList gen (o : os) index (i : is)
  | index == i = do
    expr <- subTreeMutation gen (fst o) 1
    rest <- mutateOrgList (fst $ split gen) os (index + 1) is
    return $ (expr, snd o) : rest
  | otherwise = do
    rest <- mutateOrgList gen os (index + 1) (i : is)
    return $ o : rest

reproducable :: Organism -> Bool
reproducable (_, i) = round i == 0

reproduce :: [Organism] -> [Organism]
reproduce orgs = orgs ++ [ org | org <- orgs, reproducable org ]

alifetest = do
  orgs <- generateInitPop (mkQCGen 10) 5 0.5 10
  o    <- executeOrganisms orgs
  executeOrganisms o
