module GraphicalAnalysis where

import AST
import Control.Monad
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Easy

plotSizes :: [[ASTExpression]] -> [(Int, Double)]
plotSizes exprs = zip [1 .. (length exprs)] [averageSize expr | expr <- exprs]

plotPopSizes :: [[ASTExpression]] -> [(Int, Double)]
plotPopSizes exprs = zip [1 .. (length exprs)] [fromIntegral $ length expr | expr <- exprs]

prepare :: [(String, Int)] -> [(String, Double, Bool)]
prepare = map (\(x, y) -> (x, fromIntegral y, False))

pitem :: (String, Double, Bool) -> PieItem
pitem (s, v, o) =
  pitem_value .~ v $
    pitem_label .~ s $
      pitem_offset .~ (if o then 25 else 0) $
        def

pie :: [(String, Double, Bool)] -> FilePath -> IO ()
pie values name =
  toFile def name $
    do
      pie_title .= "Relative Population"
      pie_plot . pie_data .= map pitem values

makePieCharts :: [[(String, Int)]] -> String -> IO ()
makePieCharts list name =
  forM_ [1 .. length list] $ 
    \i -> pie (prepare $ list !! (i -1)) (name ++ show i ++ ".png")

mainchart :: [[ASTExpression]] -> IO ()
mainchart exprs = toFile def "./example.png" $ do
  layout_title .= "first test"
  plot (line "Average size" [plotSizes exprs])
  plot (line "Population size" [plotPopSizes exprs])
