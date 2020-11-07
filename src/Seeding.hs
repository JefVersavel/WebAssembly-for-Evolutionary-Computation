module Seeding where

import Data.Coerce
import Data.Word
import System.Random 
import System.Random.SplitMix
import Test.QuickCheck.Gen
import Test.QuickCheck.Random

newSeed :: IO QCGen
newSeed = newQCGen

toSeed :: (Word64, Word64) -> QCGen
toSeed = coerce . seedSMGen'

fromSeed :: QCGen -> (Word64, Word64)
fromSeed = unseedSMGen . coerce

useSeed :: QCGen -> Gen a -> Gen a
useSeed r g = MkGen $ \_ n -> unGen g r n

growSeedList :: QCGen -> Int -> [QCGen]
growSeedList _ 0 = []
growSeedList gen n = g1: growSeedList g2 (n - 1)
  where
    (g1, g2) = split gen

grow2SeedLists :: QCGen -> Int -> Int -> ([QCGen], [QCGen])
grow2SeedLists gen n1 n2 = (list1, list2)
  where
    (g1, g2) = split gen
    list1 = growSeedList g1 n1
    list2 = growSeedList g2 n2

seedList :: (Num t, Enum t) => t -> IO [QCGen]
seedList n = sequence [newSeed | _ <- [1 .. n]]

doubleSeedList ::
  (Num t1, Num t2, Enum t1, Enum t2) => t1 -> t2 -> (IO [QCGen], IO [QCGen])
doubleSeedList n1 n2 = (seedList n1, seedList n2)
