{-# LANGUAGE GADTs #-}

module Environment where

import qualified Data.List as List
import Data.Matrix
import Data.Maybe
import Organism
import Seeding
import System.Random
import qualified Test.QuickCheck as QC
import Test.QuickCheck.Random

type Resource = Double

type Pos = (Int, Int)

type Lim = Pos

-- | Reprents a lifeform in the environment,
-- it can either be Nil which means that there is no life form in the cell or it can be an Organism.
data Life a = Organism a => Nil | Org a

-- | Represents a cell in the environemnt with its lifeform and a list of resources that can be found in that cell.
type Cell a = (Life a, [Resource])

instance (Show a, Organism a) => Show (Life a) where
  show Nil = "Nil"
  show (Org org) = genotype org

-- | Represents the environment with cells and the neighbourhood and limits of the environment.
data Environment a = Organism a => Env (Matrix (Cell a)) Neighbourhood Lim

instance Show a => Show (Environment a) where
  show (Env m _ _) = show m

-- | The neighbourhood of an environment which affects wich cells are neighbours of eachother.
-- https://en.wikipedia.org/wiki/Moore_neighborhood
-- https://en.wikipedia.org/wiki/Von_Neumann_neighborhood
data Neighbourhood = Moore | VonNeumann
  deriving (Show)

-- functions to work with the limits of hte environmnet

-- | Returns the limits of the environment
getLim :: Environment a -> Lim
getLim (Env _ _ l) = l

-- | Returns the limit of the 1st dimension.
getXLim :: Environment a -> Int
getXLim (Env _ _ (mx, _)) = mx

-- | Returns the limit of the 2nd dimentsion.
getYLim :: Environment a -> Int
getYLim (Env _ _ (_, my)) = my

-- | Returns the amount of cells in the environment.
getSize :: Environment a -> Int
getSize (Env _ _ (mx, my)) = mx * my

-- | Returns True if a given position is within the limits of the environment.
legalPos :: Pos -> Lim -> Bool
legalPos (x, y) (mx, my) = x >= 1 && x <= mx && y >= 1 && y <= my

-- | Returns True if the given limit is non-negative in both dimensions.
legalLimits :: Lim -> Bool
legalLimits (x, y) = x >= 0 && y >= 0

-- | Returns all the positions that make up and environment given its limit.
getAllPos :: Lim -> [Pos]
getAllPos (mx, my) = [(x, y) | x <- [1 .. mx], y <- [1 .. my]]

adjacentPos :: Pos -> [Pos]
adjacentPos (x, y) = [(x -1, y), (x + 1, y), (x, y -1), (x, y + 1)]

diagonalPos :: Pos -> [Pos]
diagonalPos (x, y) = [(x -1, y -1), (x + 1, y + 1), (x + 1, y -1), (x -1, y + 1)]

getNeighbours :: Organism a => Environment a -> Pos -> [Pos]
getNeighbours (Env _ Moore lim) p =
  [neighbour | neighbour <- adjacentPos p ++ diagonalPos p, legalPos neighbour lim]
getNeighbours (Env _ VonNeumann lim) p =
  [neighbour | neighbour <- adjacentPos p, legalPos neighbour lim]

getCellAt :: Environment a -> Pos -> Maybe (Cell a)
getCellAt (Env m _ maxPos) p
  | legalPos p maxPos = Just $ uncurry unsafeGet p m
  | otherwise = Nothing

getOrg :: Organism a => Cell a -> Maybe a
getOrg (Org o, _) = Just o
getOrg _ = Nothing

getResources :: Cell a -> Maybe [Resource]
getResources (_, r) = Just r
getResources _ = Nothing

containsResources :: Cell a -> Bool
containsResources (_, r)
  | null r = False
  | otherwise = True
containsResources _ = False

getResourcesAt :: Environment a -> Pos -> Maybe [Resource]
getResourcesAt env pos = do
  cell <- getCellAt env pos
  getResources cell

insertResourcesAt :: Environment a -> [Resource] -> Pos -> Environment a
insertResourcesAt (Env m n lim) res p
  | legalPos p lim = Env (unsafeSet (Org org, res) p m) n lim
  | otherwise = error "given position is not inbounds"
  where
    org = fromJust $ getOrgAt (Env m n lim) p

fillInResources :: Environment a -> [(Pos, [Resource])] -> Environment a
fillInResources env [] = env
fillInResources env ((pos, res) : rst) = insertResourcesAt env' res pos
  where
    env' = fillInResources env rst

isOrg :: Organism a => Cell a -> Bool
isOrg (Org _, _) = True
isOrg _ = False

getOrgAt :: Organism a => Environment a -> Pos -> Maybe a
getOrgAt env pos = do
  cell <- getCellAt env pos
  getOrg cell

getOrgsAt :: Organism a => Environment a -> [Pos] -> [(Pos, a)]
getOrgsAt _ [] = []
getOrgsAt env (x : xs) = case getCellAt env x of
  Just c ->
    if isOrg c
      then (x, fromJust (getOrg c)) : getOrgsAt env xs
      else getOrgsAt env xs
  Nothing -> getOrgsAt env xs

isNil :: Organism a => Environment a -> Pos -> Bool
isNil env p = case getCellAt env p of
  Nothing -> False
  Just (life, _) -> case life of
    Nil -> True
    _ -> False

nilGenerator :: Organism a => Pos -> Cell a
nilGenerator _ = (Nil, [])

nillify :: Organism a => Environment a -> [Pos] -> Environment a
nillify = foldl deleteOrganismAt

getNilNeighbours :: Organism a => Environment a -> Pos -> [Pos]
getNilNeighbours env p = filter (isNil env) (getNeighbours env p)

insertOrganismAt :: Organism a => Environment a -> a -> Pos -> Environment a
insertOrganismAt (Env m n lim) org p
  | legalPos p lim = Env (unsafeSet (Org org, resources) p m) n lim
  | otherwise = error "given position is not inbounds"
  where
    resources = fromJust $ getResourcesAt (Env m n lim) p

getAllOrgs :: Organism a => Environment a -> [a]
getAllOrgs (Env m _ _) = map (fromJust . getOrg) $ filter isOrg $ toList m

getOrgsPos :: Organism a => Environment a -> [(Pos, a)]
getOrgsPos env = getOrgsAt env $ getAllPos $ getLim env

deleteOrganismAt :: Organism a => Environment a -> Pos -> Environment a
deleteOrganismAt (Env m n lim) p
  | legalPos p lim = Env (unsafeSet (Nil, resources) p m) n lim
  | otherwise = error "given position is not inbounds"
  where
    resources = fromJust $ getResourcesAt (Env m n lim) p

fillInOrgs :: Organism a => Environment a -> [(Pos, a)] -> Environment a
fillInOrgs env [] = env
fillInOrgs env ((pos, org) : rst) = insertOrganismAt env' org pos
  where
    env' = fillInOrgs env rst

empty :: Organism a => Lim -> Neighbourhood -> Environment a
empty (mx, my) n
  | legalLimits (mx, my) = Env (matrix mx my nilGenerator) n (mx, my)
  | otherwise = error "the given limits must be positive and nonzero"

distribute :: QCGen -> [a] -> Lim -> IO [(Pos, a)]
distribute gen orgs lim = distribute' gen orgs $ getAllPos lim

distribute' :: QCGen -> [a] -> [Pos] -> IO [(Pos, a)]
distribute' _ [] _ = return []
distribute' gen (o : os) list = do
  let (g1, g2) = split gen
  p <- QC.generate $ useSeed g1 $ QC.elements list
  let l = List.delete p list
  rest <- distribute' g2 os l
  return $ (p, o) : rest

initializeEnvironment ::
  Organism a =>
  Neighbourhood ->
  QCGen ->
  [a] ->
  Lim ->
  IO (Environment a)
initializeEnvironment n gen orgList lim = do
  let (g1, g2) = split gen
  posOrgs <- distribute gen orgList lim
  let env = fillInOrgs (empty lim n) posOrgs
  let amount = floor $ (fromIntegral (getSize env) :: Double) * 0.1
  let resources = generateResources g1 amount 3
  posRes <- distribute g2 resources lim
  return $ fillInResources env posRes

selectPosition :: QCGen -> [Pos] -> IO Pos
selectPosition gen positions = QC.generate $ useSeed gen $ QC.elements positions

generateRandomPositions :: Organism a => QCGen -> Environment a -> Int -> [Pos]
generateRandomPositions gen env n = take n $ List.nub $ genRanPos gen env

genRanPos :: Organism a => QCGen -> Environment a -> [Pos]
genRanPos gen env = (x, y) : genRanPos g2 env
  where
    mx = getXLim env
    my = getYLim env
    (x, g1) = randomR (1, mx) gen
    (y, g2) = randomR (1, my) g1

-- | Generates a list of a givne length of lists of randomly generated resources
-- with a random length no greater than a given maximum size.
generateResources :: QCGen -> Int -> Int -> [[Resource]]
generateResources _ 0 _ = []
generateResources gen amount m = take randomSize infinteList : generateResources g3 (amount - 1) m
  where
    (g1, g2) = split gen
    (randomSize, g3) = randomR (1, m) g1
    infinteList = randoms g2
