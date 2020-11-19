{-# LANGUAGE GADTs #-}

module Environment where

import qualified Data.List as List
import Data.Matrix
import Data.Maybe
import Organism
import Seeding
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Random

type Resource = Double

type Pos = (Int, Int)

type Lim = Pos

data Place a = Organism a => Nil | Org a | Res Resource

instance (Show a, Organism a) => Show (Place a) where
  show Nil = "Nil"
  show (Org org) = genotype org
  show (Res r) = "Res"

data Environment a = Organism a => Env (Matrix (Place a)) Neighbourhood Lim

instance Show a => Show (Environment a) where
  show (Env m _ _) = show m

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

getplaceAt :: Environment a -> Pos -> Maybe (Place a)
getplaceAt (Env m _ maxPos) p
  | legalPos p maxPos = Just $ uncurry unsafeGet p m
  | otherwise = Nothing

getOrg :: Organism a => Place a -> Maybe a
getOrg (Org o) = Just o
getOrg _ = Nothing

getResource :: Place a -> Maybe Resource
getResource (Res r) = Just r
getResource _ = Nothing

isResource :: Place a -> Bool
isResource (Res _) = True
isResource _ = False

isOrg :: Organism a => Place a -> Bool
isOrg (Org _) = True
isOrg _ = False

getOrgsAt :: Organism a => Environment a -> [Pos] -> [(Pos, a)]
getOrgsAt _ [] = []
getOrgsAt env (x : xs) = case getplaceAt env x of
  Just p ->
    if isOrg p
      then (x, fromJust (getOrg p)) : getOrgsAt env xs
      else getOrgsAt env xs
  Nothing -> getOrgsAt env xs

getResourceAt :: Environment a -> Pos -> Maybe Resource
getResourceAt env pos = do
  place <- getplaceAt env pos
  getResource place

isNil :: Organism a => Environment a -> Pos -> Bool
isNil env p = case getplaceAt env p of
  Nothing -> False
  Just pl -> case pl of
    Nil -> True
    _ -> False

nilGenerator :: Organism a => Pos -> Place a
nilGenerator _ = Nil

nillify :: Organism a => Environment a -> [Pos] -> Environment a
nillify = foldl deleteOrganismAt

getNilNeighbours :: Organism a => Environment a -> Pos -> [Pos]
getNilNeighbours env p = filter (isNil env) (getNeighbours env p)

insertOrganismAt :: Organism a => Environment a -> a -> Pos -> Environment a
insertOrganismAt (Env m n lim) org p
  | legalPos p lim = Env (unsafeSet (Org org) p m) n lim
  | otherwise = error "given position is not inbounds"

getAllOrgs :: Organism a => Environment a -> [a]
getAllOrgs (Env m _ _) = map (fromJust . getOrg) $ filter isOrg $ toList m

getOrgsPos :: Organism a => Environment a -> [(Pos, a)]
getOrgsPos env = getOrgsAt env $ getAllPos $ getLim env

deleteOrganismAt :: Organism a => Environment a -> Pos -> Environment a
deleteOrganismAt (Env m n lim) p
  | legalPos p lim = Env (unsafeSet Nil p m) n lim
  | otherwise = error "given position is not inbounds"

fillInOrgs :: Organism a => Environment a -> [(Pos, a)] -> Environment a
fillInOrgs env [] = env
fillInOrgs env ((pos, org) : rst) = insertOrganismAt env' org pos
  where
    env' = fillInOrgs env rst

empty :: Organism a => Lim -> Neighbourhood -> Environment a
empty (mx, my) n
  | legalLimits (mx, my) = Env (matrix mx my nilGenerator) n (mx, my)
  | otherwise = error "the given limits must be positive and nonzero"

distributeOrgs :: Organism a => QCGen -> [a] -> Lim -> IO [(Pos, a)]
distributeOrgs gen orgs lim = distributeOrgs' gen orgs $ getAllPos lim

distributeOrgs' :: Organism a => QCGen -> [a] -> [Pos] -> IO [(Pos, a)]
distributeOrgs' _ [] _ = return []
distributeOrgs' gen (o : os) list = do
  let (g1, g2) = split gen
  p <- generate $ useSeed g1 $ elements list
  let l = List.delete p list
  rest <- distributeOrgs' g2 os l
  return $ (p, o) : rest

initializeEnvironment ::
  Organism a =>
  Neighbourhood ->
  QCGen ->
  [a] ->
  Lim ->
  IO (Environment a)
initializeEnvironment n gen orgList lim = do
  posOrgs <- distributeOrgs gen orgList lim
  return $ fillInOrgs (empty lim n) posOrgs

selectPosition :: QCGen -> [Pos] -> IO Pos
selectPosition gen positions = generate $ useSeed gen $ elements positions

generateRandomPositions :: Organism a => QCGen -> Environment a -> Int -> [Pos]
generateRandomPositions gen env n = take n $ List.nub $ genRanPos gen env

-- Creates infinite list of positions in the environment so be careful
genRanPos :: Organism a => QCGen -> Environment a -> [Pos]
genRanPos gen env = (x, y) : genRanPos g2 env
  where
    mx = getXLim env
    my = getYLim env
    (x, g1) = randomR (1, mx) gen
    (y, g2) = randomR (1, my) g1
