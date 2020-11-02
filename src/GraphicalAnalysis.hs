module GraphicalAnalysis where

import AST
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Easy

plotSizes exprs = zip [1 .. (length exprs)] [averageSize expr | expr <- exprs]

plotPopSizes exprs = zip [1 .. (length exprs)] [fromIntegral $ length expr | expr <- exprs]

mainchart exprs = toFile def "./example.png" $ do
  layout_title .= "first test"
  plot (line "Average size" [plotSizes exprs])
  plot (line "Population size" [plotPopSizes exprs])
