{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import ALife
import qualified Data.ByteString.Lazy as B
import Data.Csv as C
import qualified Data.Vector as V
import GHC.Generics
import System.Directory

data TestData = TestData
  { seed :: Int,
    start :: Double,
    iterations :: Int,
    limits :: Int,
    depth :: Int,
    mutationRate :: Int,
    divider :: Int
  }
  deriving (Show, Generic)

instance FromRecord TestData

instance ToRecord TestData

executeTestdata :: [TestData] -> IO ()
executeTestdata [] = return ()
executeTestdata (TestData se st it li de mu di : rest) = do
  mainCreature se st it li de mu di
  executeTestdata rest

main :: IO ()
main = do
  contents <- listDirectory "./csv"
  file <- B.readFile $ "./csv/" ++ head contents
  let decoded = decode HasHeader file :: Either String (V.Vector TestData)
  case decoded of
    Left str -> print str
    Right v -> do
      executeTestdata $ V.toList v
  print decoded
  print contents
