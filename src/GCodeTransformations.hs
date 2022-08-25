module GCodeTransformations (toOrigin, setOutputSize) where

import Data.GCode (updateLimitsCode, AxisDesignator (X, Y), modifyXY)
import Data.GCode.Types (GCode)
import qualified Data.Map as M

toOrigin :: GCode -> GCode
toOrigin code = do
  let ((xmin, _), (ymin, _)) = findLimits code
  let mov x y = (x - xmin, y - ymin)
  map (modifyXY mov) code

setOutputSize :: (Maybe Double, Maybe Double) -> GCode -> GCode
setOutputSize (Nothing, Nothing) code = code
setOutputSize outputSize code = do
  let ((_, xmax), (_, ymax)) = findLimits code
  let mov x y = case outputSize of
        (Just xSize, Nothing) -> do
          let scale = xSize / xmax
          (x * scale, y * scale)
        (Nothing, Just ySize) -> do
          let scale = ySize / ymax
          (x * scale, y * scale)
        (Just xSize, Just ySize) -> do
          let xScale = xSize / xmax
          let yScale = ySize / ymax
          (x * xScale, y * yScale)
  map (modifyXY mov) code

findLimits :: GCode -> ((Double, Double), (Double, Double))
findLimits code = case (M.lookup X lim, M.lookup Y lim) of
                    (Just x, Just y) -> (x, y)
                    _ -> error "failed to find limits"
  where
    lim = foldl updateLimitsCode M.empty code
