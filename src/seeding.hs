
module Seeding where

import Data.Coerce
import Data.Word
import System.Random.SplitMix
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import System.Random (RandomGen(next))

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
growSeedList gen n = gen : growSeedList (snd $ next gen) (n-1)