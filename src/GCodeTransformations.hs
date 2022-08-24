module GCodeTransformations (toOrigin) where

import Data.GCode (updateLimitsCode, AxisDesignator (X, Y), modifyXY)
import Data.GCode.Types (GCode)
import qualified Data.Map as M

toOrigin :: GCode -> GCode
toOrigin code = do
  let lim = foldl updateLimitsCode  M.empty code
  case (M.lookup X lim, M.lookup Y lim) of
      (Just (xmin, _), Just (ymin, _)) -> do
        let mov x y = (x - xmin, y - ymin)
        map (modifyXY mov) code
      _ -> error "failed to find limits"
