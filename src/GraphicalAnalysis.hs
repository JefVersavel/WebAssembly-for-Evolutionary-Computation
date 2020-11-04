module GraphicalAnalysis where

import AST
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Easy

plotSizes exprs = zip [1 .. (length exprs)] [averageSize expr | expr <- exprs]

plotPopSizes exprs = zip [1 .. (length exprs)] [fromIntegral $ length expr | expr <- exprs]

prepareForBar :: [(x, y)] -> ([x], [[y]])
prepareForBar d = (getFirsts d, getSeconds d)

getFirsts :: [(x, y)] -> [x]
getFirsts = map fst

getSeconds :: [(x, y)] -> [[y]]
getSeconds = map (\ x -> [snd x])

barChart genoData = toRenderable layout
  where
    layout =
      layout_title .~ "Sample Bars" $
        layout_title_style . font_size .~ 10 $
          layout_x_axis . laxis_generate .~ autoIndexAxis alabels $
            layout_y_axis . laxis_override .~ axisGridHide $
              layout_left_axis_visibility . axis_show_ticks .~ False $
                layout_plots .~ [plotBars bars2] $
                  def ::
        Layout PlotIndex Int
    bars2 =
      plot_bars_titles .~ ["Populationsize"] $
        plot_bars_values .~ addIndexes (snd genoData) $
          plot_bars_style .~ BarsClustered $
            plot_bars_spacing .~ BarsFixGap 30 5 $
              plot_bars_item_styles .~ map mkstyle (cycle defaultColorSeq) $
                def
    alabels = fst genoData
    mkstyle c = (solidFillStyle c, Nothing)

makeBarChart :: [(String, Int)] -> String -> IO ()
makeBarChart d name = do
  let barData = prepareForBar d
  _ <- renderableToFile def (name ++ ".png") (barChart barData)
  return ()

mainchart exprs = toFile def "./example.png" $ do
  layout_title .= "first test"
  plot (line "Average size" [plotSizes exprs])
  plot (line "Population size" [plotPopSizes exprs])
