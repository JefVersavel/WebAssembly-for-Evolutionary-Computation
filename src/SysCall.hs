module SysCall where

-- | Represents the call that an organism can make to the host.
-- The calls are then performed by the host.
data SysCall = None | Reproduction
  deriving (Bounded, Enum, Show, Eq, Ord)

-- | Returns the syscall that is associated with the given outcome of the orgamism
toSysCall :: Double -> SysCall
toSysCall n = toEnum (round (abs n) `rem` s)
  where
    s = length [minBound :: SysCall .. maxBound]
