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

seeds = [1 .. 50]

starts = [1 .. 10]

main :: IO [()]
main = do
  sequence [mainCreature seed 10 100 5 5 2 20 5 start | seed <- seeds, start <- starts]
