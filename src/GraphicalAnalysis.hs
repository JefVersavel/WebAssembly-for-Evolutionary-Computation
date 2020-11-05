module GraphicalAnalysis where

import AST
import Control.Monad
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Easy
import Plots

plotSizes exprs = zip [1 .. (length exprs)] [averageSize expr | expr <- exprs]

plotPopSizes exprs = zip [1 .. (length exprs)] [fromIntegral $ length expr | expr <- exprs]

prepareForBar :: [(String, Int)] -> [(String, Double, Bool)]
prepareForBar d = map (\(x, y) -> (x, fromIntegral y, False)) d

pitem (s, v, o) =
  pitem_value .~ v $
    pitem_label .~ s $
      pitem_offset .~ (if o then 25 else 0) $
        def

--rChart genoData name = toFile def name $ do
--let prepared = prepareForBar genoData
--layout_title .= "Sample Bars"
--layout_title_style . font_size .= 10
--layout_x_axis . laxis_generate .= autoIndexAxis (map fst prepared)
--plot $ fmap plotBars $ bars ["populationseze"] (addIndexes (map snd prepared))

pie values name =
  toFile def name $
    do
      pie_title .= "Relative Population"
      pie_plot . pie_data .= map pitem values

makePieCharts list name = forM_ [1 .. length list] $ \i -> pie (prepareForBar $ list !! (i -1)) (name ++ show i)

mainchart exprs = toFile def "./example.png" $ do
  layout_title .= "first test"
  plot (line "Average size" [plotSizes exprs])
  plot (line "Population size" [plotPopSizes exprs])
