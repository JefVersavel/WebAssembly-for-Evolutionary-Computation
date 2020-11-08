{-# LANGUAGE GADTs #-}

module Environment where

import Data.Matrix
import Data.Maybe
import qualified Data.Set as Set
import Organism
import Seeding
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Random

type Resource = Double

type Pos = (Int, Int)

data Place a = Organism a => Nil | Org a | Res Resource

data Environment a = Organism a => Env (Matrix (Place a)) Neighbourhood Pos

-- https://en.wikipedia.org/wiki/Moore_neighborhood
-- https://en.wikipedia.org/wiki/Von_Neumann_neighborhood
data Neighbourhood = Moore | VonNeumann

legalPos :: Pos -> Pos -> Bool
legalPos (x, y) (mx, my) = x >= 1 && x <= mx && y >= 1 && y <= my

legalLimits :: Pos -> Bool
legalLimits (x, y) = x >= 0 && y >= 0

getAllPos :: Pos -> [Pos]
getAllPos (mx, my) = [(x, y) | x <- [1 .. mx], y <- [1 .. my]]

getOrg :: Organism a => Place a -> Maybe a
getOrg (Org o) = Just o
getOrg _ = Nothing

isOrg :: Organism a => Place a -> Bool
isOrg (Org o) = True
isOrg _ = False

isNil :: Organism a => Place a -> Bool
isNil Nil = True
isNil _ = False

getplaceAt :: Organism a => Environment a -> Pos -> Maybe (Place a)
getplaceAt (Env m _ maxPos) p
  | legalPos p maxPos = Just $ uncurry unsafeGet p m
  | otherwise = Nothing

nilGenerator :: Organism a => Pos -> Place a
nilGenerator _ = Nil

insertOrganismAt :: Organism a => Environment a -> a -> Pos -> Environment a
insertOrganismAt (Env m n lim) org p
  | legalPos p lim = Env (unsafeSet (Org org) p m) n lim
  | otherwise = error "given position is not inbounds"

adjacentPos :: Pos -> [Pos]
adjacentPos (x, y) = [(x -1, y), (x + 1, y), (x, y -1), (x, y + 1)]

diagonalPos :: Pos -> [Pos]
diagonalPos (x, y) = [(x -1, y -1), (x + 1, y + 1), (x + 1, y -1), (x -1, y + 1)]

getNeighbours :: Organism a => Environment a -> Pos -> [Pos]
getNeighbours (Env _ Moore lim) p =
  [neighbour | neighbour <- adjacentPos p ++ diagonalPos p, legalPos neighbour lim]
getNeighbours (Env _ VonNeumann lim) p =
  [neighbour | neighbour <- adjacentPos p, legalPos neighbour lim]

getAllOrgs :: Organism a => Environment a -> [a]
getAllOrgs (Env m _ _) = map (fromJust . getOrg) $ filter isOrg $ toList m

empty :: Organism a => Pos -> Neighbourhood -> Environment a
empty (mx, my) n
  | legalLimits (mx, my) = Env (matrix mx my nilGenerator) n (mx, my)
  | otherwise = error "the given limits must be positive and nonzero"

distributeOrgs :: Organism a => QCGen -> [a] -> Pos -> IO [(Pos, a)]
distributeOrgs gen orgs lim = distributeOrgs' gen orgs $ Set.fromList $ getAllPos lim

distributeOrgs' :: Organism a => QCGen -> [a] -> Set.Set Pos -> IO [(Pos, a)]
distributeOrgs' gen (o : os) set = do
  let (g1, g2) = split gen
  p <- generate $ useSeed g1 $ elements $ Set.toList set
  let s = Set.delete p set
  rest <- distributeOrgs' g2 os s
  return $ (p, o) : rest
