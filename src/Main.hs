{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import ALife
import qualified Data.ByteString.Lazy as B
import Data.Csv as C
import Data.List.Split
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

runMultipleFiles :: [FilePath] -> IO ()
runMultipleFiles [] = return ()
runMultipleFiles (f : rest)
  | last (splitOn "." f) == "csv" = do
    putStr "\n\n\n\n\n\n"
    putStr f
    file <- B.readFile $ "./csv/" ++ f
    let decoded = decode HasHeader file :: Either String (V.Vector TestData)
    case decoded of
      Left str -> print str
      Right v -> do
        executeTestdata $ V.toList v
        runMultipleFiles rest
  | otherwise = runMultipleFiles rest

main :: IO ()
main = do
  contents <- listDirectory "./csv"
  runMultipleFiles contents
