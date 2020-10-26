{-# LANGUAGE ScopedTypeVariables #-}

module ALife_1 where

import           AST
import qualified Data.ByteString        as BS
import           Data.List
import           ExecuteWasm
import           Generators
import           GeneticOperations
import           System.Random
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Random
import           WasmGenerator


data Organism = Organism {
  expression :: ASTExpression,
  bytes      :: BS.ByteString,
  register   :: Double
                         }

instance Show Organism where
  show (Organism e _ r) = smallShow e ++ " " ++ show r ++ "\n"

generateInitPop :: QCGen -> Int -> Double -> Int -> Double -> IO [Organism]
generateInitPop gen d ratio n start = do
  expr       <- sequence [ generate g | g <- rampedHalfNHalf gen d 1 ratio n ]
  serialized <- sequence [ serializeExpression e [start] | e <- expr ]
  return [ Organism e s start | (e, s) <- zip expr serialized ]

executeOrganism :: Organism -> IO Organism
executeOrganism (Organism e b _) = do
  outcome <- executeModule b
  return $ Organism e b outcome

executeOrganisms :: [Organism] -> IO [Organism]
executeOrganisms orgs = sequence [ executeOrganism org | org <- orgs ]

mutateOrganisms :: QCGen -> [Organism] -> Double -> IO [Organism]
mutateOrganisms _   orgs 0     = return orgs
mutateOrganisms gen orgs ratio = do
  let n       = round $ ratio * fromIntegral (length orgs)
  let nextGen = fst $ split gen
  let indexes = take n $ nub $ randomRs (0, length orgs - 1) nextGen
  mutateOrgList gen orgs 0 indexes

mutateOrgList :: QCGen -> [Organism] -> Int -> [Int] -> IO [Organism]
mutateOrgList _ orgs _ [] = return orgs
mutateOrgList _ []   _ _  = return []
mutateOrgList gen (o : os) index (i : is)
  | index == i = do
    let reg = register o
    expr       <- subTreeMutation gen (expression o) 1
    rest       <- mutateOrgList (fst $ split gen) os (index + 1) is
    serialized <- serializeExpression expr [reg]
    return $ Organism expr serialized reg : rest
  | index > i = do
    rest <- mutateOrgList gen os (index + 1) is
    return $ o : rest
  | otherwise = do
    rest <- mutateOrgList gen os (index + 1) (i : is)
    return $ o : rest

killRandom :: [Organism] -> QCGen -> [Organism]
killRandom []  _ = []
killRandom [o] _ = [o]
killRandom (o : os) gen | kill      = killRandom os nextGen
                        | otherwise = o : killRandom os nextGen
  where (kill :: Bool, nextGen) = random gen

killFirst :: [Organism] -> Double -> [Organism]
killFirst orgs ratio = drop n orgs
  where n = floor $ ratio * fromIntegral (length orgs)

reproducable :: Organism -> Bool
reproducable org = round (register org) `mod` (7 :: Int) == 1

reproduce :: [Organism] -> [Organism]
reproduce orgs = orgs ++ [ org | org <- orgs, reproducable org ]

run :: [Organism] -> QCGen -> Double -> Int -> Int -> IO [Organism]
run orgs _   _     _ 0 = return orgs
run orgs gen ratio m n = do
  print "###################################################################"
  print orgs
  let (g1, g2) = split gen
  executed <- executeOrganisms orgs
  mutated  <- mutateOrganisms g1 executed ratio
  let (g11, g22) = split g2
  if length mutated > m
    then run (reproduce $ killRandom mutated g11) g22 ratio m (n - 1)
    else run (reproduce mutated) g22 ratio m (n - 1)

main :: IO ()
main = do
  orgs <- generateInitPop (mkQCGen 10) 5 0.5 10 4
  print orgs
  print "###################################################################"
  --return $ killRandom orgs (mkQCGen 15465655600)
  finalOrgs <- run orgs (mkQCGen 10) 0.5 10 10
  print "###################################################################"
  print finalOrgs
