module SysCall where

import AST
import Seeding
import Test.QuickCheck
import Test.QuickCheck.Random

-- | Represents the call that an organism can make to the host.
-- The calls are then performed by the host.
data SysCall
  = Reproduction
  | None
  | Up
  | Down
  | Rght
  | Lft
  deriving (Bounded, Enum, Show, Eq, Ord)

-- | Returns the syscall that is associated with the given outcome of the orgamism
toSysCall :: Double -> SysCall
toSysCall n = toEnum (round (abs n) `rem` s)
  where
    s = length [minBound :: SysCall .. maxBound]

decideSysCall :: Double -> ASTExpression -> Bool -> IO SysCall
decideSysCall n expr plnt
  | plnt =
    generate $
      useSeed gen $
        frequency
          [(sze, return Reproduction), (dpth, return None)]
  | otherwise =
    generate $
      useSeed gen $
        frequency
          [(sze, return Reproduction), (sze - dpth, elements [Up, Down, Rght, Lft]), (dpth, return None)]
  where
    gen = mkQCGen $ round n
    dpth = getMaxDepth expr
    sze = size expr
