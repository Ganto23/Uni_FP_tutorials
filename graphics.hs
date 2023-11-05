module Main where

import qualified Data.Vector as V
import Graphics.Rendering.Cairo
import Linear.V2

newtype Contour =
  Contour (V.Vector (V2 Double))

contourPath :: Contour -> Render ()
contourPath (Contour vertices) = foldr1 (>>) $ concat [initCmds, lines, endCmds]
  where
    initCmds = [newPath, moveTo (startX) (startY)]
    lines = V.toList $ V.map (\(V2 x y) -> lineTo x y) $ V.tail vertices
    endCmds = [closePath]
    V2 startX startY = V.head vertices

bg :: Render ()
bg = do
  setSourceRGBA 0 0 0 1
  rectangle 0 0 500 500
  fill

drawTriangle :: Render ()
drawTriangle = do
  setSourceRGBA 1 1 0 1
  contourPath $ Contour $ V.fromList [V2 10 10, V2 100 10, V2 100 100]
  fill

sketch :: Render ()
sketch = bg >> drawTriangle

main :: IO ()
main = do
  surface <- createImageSurface FormatARGB32 500 500
  renderWith surface sketch
  surfaceWriteToPNG surface "out.png"