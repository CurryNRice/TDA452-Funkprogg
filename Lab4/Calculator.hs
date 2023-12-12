-- This module is a starting point for implementing the Graph Drawing
-- Calculator as described in Part II of the Standard Lab. You can use this
-- directly, or just study it as an example of how to use threepenny-gui.

import ThreepennyPages
import Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny as UI
import Expr
import Data.IORef
import Data.Maybe

canWidth,canHeight :: Num a => a
canWidth  = 300
canHeight = 300

scale = 0.04

main :: IO ()
main = startGUI defaultConfig setup



setup :: Window -> UI ()
setup window =
  do -- Create them user interface elements
     canvas  <- mkCanvas canWidth canHeight   -- The drawing area
     fx      <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
     input   <- mkInput 20 "x"                -- The formula input
     draw    <- mkButton "Draw graph"         -- The draw button
     zoomTxt <- mkHTML "<i>Zoom:</i>"         -- The slider text
     zoom    <- mkSlider (1,5) 1              -- The zoom slider
     diff    <- mkButton "d/dx="              -- The diffenentiate button
       -- The markup "<i>...</i>" means that the text inside should be rendered
       -- in italics.

     -- Add the user interface elements to the page, creating a specific layout
     formula <- row [pure fx,pure input]
     slider <- row [pure zoomTxt, pure zoom]
     getBody window #+ [column [pure canvas,pure formula,pure draw, pure slider, pure diff]]

     -- Styling
     getBody window # set style [("backgroundColor","lightblue"),
                                 ("textAlign","center")]
     pure input # set style [("fontSize","14pt")]

     -- Interaction (install event handlers)
     on UI.click     draw  $ \ _ -> readAndDraw input scale canvas

     -- Differantiate button event
     on UI.click     diff  $ \ _ -> do
          formula <- get value input
          pure input # set value (clean $ diffr formula) 
          readAndDraw input scale canvas

     -- Zoom event 
     on valueChange' zoom  $ \ _ -> do
          zoomVal <- get value zoom 
          let zoomVal' = read zoomVal
          let scale'   = scale/zoomVal'
          readAndDraw input scale' canvas
     on valueChange' input $ \ _ -> readAndDraw input scale canvas
     where 
          clean s = showExpr $ simplify $ fromJust $ readExpr s
          diffr formula = showExpr $ differentiate $ fromJust $ readExpr formula

readAndDraw :: Element -> Double -> Canvas -> UI ()
readAndDraw input scalor canvas =
  do -- Get the current formula (a String) from the input element
     formula <- get value input
     let expr = case readExpr formula of 
                          (Just e) -> e
                          Nothing -> error ("Problem in the readDraw formula was" ++ formula)
     -- Clear the canvas
     clearCanvas canvas
     -- The following code draws the formula as a graph
     path "blue" (points expr scalor (canWidth, canHeight)) canvas


-- | Creates poins for all values (x, y) where y is the evaluated expression at x. 
-- | It also take into account the scale d 
points :: Expr -> Double -> (Int,Int) -> [Point]
points e d (w,h) = [mathToPix (createCoord x) | x <- [(-wm), ((-wm)+d) .. wm ]]
  where 
     w' = fromIntegral w
     wm = w' * d 
     createCoord x = (x, eval e x)
     mathToPix (x, y) = (x/d + w'/2, (-y)/d+w'/2)


