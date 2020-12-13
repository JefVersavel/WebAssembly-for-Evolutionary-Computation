module Experiments where

import ALife_1

--
experiment1 :: IO ()
experiment1 = mainMVP 10 5 0.5 0.5 10 100 10

experiment2 :: IO ()
experiment2 = mainMVP 11 10 0.5 0.5 10 100 10

experiment3 :: IO ()
experiment3 = mainMVP 12 5 0.9 0.5 10 100 10

experiment4 :: IO ()
experiment4 = mainMVP 13 5 0.5 0 10 100 10

experiment5 :: IO ()
experiment5 = mainMVP 14 5 0.5 0.5 30 100 10

experiment6 :: IO ()
experiment6 = mainMVP 15 5 0.5 0.5 10 100 0

experiment7 :: IO ()
experiment7 = mainMVP 16 5 0.5 1 10 100 10

experiment8 :: IO ()
experiment8 = mainMVP 17 5 0.5 0.5 10 100 6547

runExperiments :: IO ()
runExperiments = do
  experiment1
  experiment2
  experiment3
  experiment4
  experiment5
  experiment6
  experiment7
  experiment8
