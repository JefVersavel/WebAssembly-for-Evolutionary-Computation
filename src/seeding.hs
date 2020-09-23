
module Seeding where
    
import Data.Coerce
import Data.Word
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