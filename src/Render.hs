module Render ( renderDoc
              ) where

import qualified Graphics.Svg as SVG
import qualified Graphics.Svg.CssTypes as CSS
import qualified Linear

import Types
import Transformation
import SvgArcSegment
import Approx
import SVGExt

import qualified CircularArc as CA
import qualified BiArc as BA
import qualified CubicBezier as B
import Data.GCode.RS274 ( rapid, move, arcCW, arcCCW)
import Data.GCode.Generate ( xy, ij, z, feed )
import Data.GCode.Types ( (&), GCode)

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

fromSvgPoint :: Int -> SVG.Point -> Point
fromSvgPoint dpi (x,y) = (fromSvgNumber dpi x, fromSvgNumber dpi y)

fromRPoint :: SVG.RPoint -> Point
fromRPoint (Linear.V2 x y) = (x, y)

toPoint :: Linear.V2 Double -> Point
toPoint (Linear.V2 x y) = (x, y)

fromPoint :: Point -> Linear.V2 Double
fromPoint (x, y) = Linear.V2 x y

-- TODO: em, percentage
fromSvgNumber :: Int -> SVG.Number -> Double
fromSvgNumber dpi num = fromNumber' (CSS.toUserUnit dpi num)
    where
        fromNumber' (SVG.Num n) = n
        fromNumber' _ = error "TODO: unhandled em or percentage"

-- current point + control point -> mirrored control point
mirrorControlPoint :: Point -> Point -> Point
mirrorControlPoint (cx, cy) (cpx, cpy) = (cx + cx - cpx, cy + cy - cpy)

-- convert a quadratic bezier to a cubic one
bezierQ2C :: Point -> Point -> Point -> DrawOp
bezierQ2C (qp0x, qp0y) (qp1x, qp1y) (qp2x, qp2y)
    = DBezierTo (qp0x + 2.0 / 3.0 * (qp1x - qp0x), qp0y + 2.0 / 3.0 * (qp1y - qp0y))
                (qp2x + 2.0 / 3.0 * (qp1x - qp2x), qp2y + 2.0 / 3.0 * (qp1y - qp2y))
                (qp2x, qp2y)

toAbsolute :: (Double, Double) -> SVG.Origin -> (Double, Double) -> (Double, Double)
toAbsolute _ SVG.OriginAbsolute p = p
toAbsolute (cx,cy) SVG.OriginRelative (dx,dy) = (cx+dx, cy+dy)

docTransform :: Int -> SVG.Document -> TransformationMatrix
docTransform dpi doc = multiply mirrorTransform (viewBoxTransform $ SVG._viewBox doc)
    where
        viewBoxTransform (Just (vbx,vby,vbw,vbh))
            = multiply (scaleTransform (w/vbw) (h/vbh)) (translateTransform (-vbx) (-vby))
        viewBoxTransform Nothing
            = identityTransform

        mirrorTransform = mirrorYTransform w h

        (w, h) = documentSize dpi doc

dd :: Int -> Double
dd = fromIntegral

mm :: Double -> Int -> Double
mm px dpi = (px * 2.54 * 10) / dd dpi

mmP :: Point -> Int -> Point
mmP (x,y) dpi = (mm x dpi, mm y dpi)

-- toolOn :: GCode
-- toolOn = [rapid & z (-0.125) & feed 2200.0]

-- toolOff :: GCode
-- toolOff = [rapid & z 5.000]

feedrate :: Double
feedrate = 13000.0

renderDoc :: Bool -> Int -> Double -> SVG.Document -> MachineSettings -> GCode
renderDoc generateBezier dpi resolution doc (MachineSettings _ _ toolOn toolOff)
    = stage2 $ renderTrees (docTransform dpi doc) (SVG._elements doc)
    where
        pxresolution = fromIntegral dpi / 2.45 / 10 * resolution

        -- TODO: make it tail recursive
        stage2 :: [DrawOp] -> GCode
        stage2 dops = convert dops (Linear.V2 0 0) True
            where
                convert [] _ _ = []
                convert (DMoveTo p:ds) _ False = (rapid & uncurry xy (mmP p dpi)) : convert ds (fromPoint p) False
                convert (DMoveTo p:ds) _ True = toolOff ++ [rapid & uncurry xy (mmP p dpi)] ++ convert ds (fromPoint p) False
                convert gs cp False = toolOn ++ convert gs cp True
                convert (DLineTo p:ds) _ True = move & uncurry xy (mmP p dpi) : convert ds (fromPoint p) True
                convert (DBezierTo c1 c2 p2:ds) cp _
                    | generateBezier
                        = error "TODO: Bezier curves are undefined"
                        -- = g <#> 5 & ij 1.0 2.0 & xy 3.0 4.0  : convert ds (fromPoint p2) True
                        -- = GBezierTo c1 c2 p2 : convert ds (fromPoint p2)
                    | otherwise
                        = fst biarcCodes ++ convert ds (fromPoint p2) True
                    where
                        biarcs = bezier2biarc (B.CubicBezier cp (fromPoint c1) (fromPoint c2) (fromPoint p2)) pxresolution
                       
                        biarc2garc (Left biarc)
                            = [Left (BA._a1 biarc), Left (BA._a2 biarc)]
                        biarc2garc (Right (Linear.V2 x y))
                            = [Right (move & uncurry xy (mmP (x, y) dpi), (x, y))]

                        biarcs2Code (codes, previousPoint) (Left arc) = do
                            let (code, nextPoint) = arc2garc previousPoint arc
                            (codes ++ [code], nextPoint)
                        biarcs2Code (codes, _) (Right (code, nextPoint)) = (codes ++ [code], nextPoint)

                        arc2garc previousPoint arc
                            -- initial implementation was mirrored
                            | not $ CA.isClockwise arc = (arcCW & uncurry xy centerPoint & uncurry ij endPoint, nextPoint)
                            | otherwise = (arcCCW & uncurry xy centerPoint & uncurry ij endPoint, nextPoint)
                            where
                                nextPoint = toPoint (CA._p2 arc)
                                (cx, cy) = mmP previousPoint dpi
                                (ox, oy) = mmP (toPoint (CA._c arc)) dpi
                                centerPoint = mmP nextPoint dpi

                                i = ox - cx
                                j = oy - cy

                                endPoint = (i, j)

                        biarcCodes = foldl biarcs2Code ([], toPoint cp) $  concatMap biarc2garc biarcs

        renderPathCommands :: Point -> Point -> Maybe Point -> [SVG.PathCommand] -> [DrawOp]
        renderPathCommands _ currentp _ (SVG.MoveTo origin (p:ps):ds)
            = DMoveTo ap : renderPathCommands ap ap Nothing (cont ps)
            where
                ap = toAbsolute currentp origin (fromRPoint p)

                cont [] = ds
                cont ps' = SVG.LineTo origin ps' : ds

        renderPathCommands firstp currentp _ (SVG.LineTo origin (p:ps):ds)
            = DLineTo ap : renderPathCommands firstp ap Nothing (cont ps)
            where
                ap = toAbsolute currentp origin (fromRPoint p)

                cont [] = ds
                cont ps' = SVG.LineTo origin ps' : ds

        renderPathCommands firstp (_, cy) _ (SVG.HorizontalTo SVG.OriginAbsolute (px:pxs):ds)
            = DLineTo ap : renderPathCommands firstp ap Nothing (cont pxs)
            where
                ap = (px,cy)

                cont [] = ds
                cont pxs' = SVG.HorizontalTo SVG.OriginAbsolute pxs' : ds

        renderPathCommands firstp (cx, cy) _ (SVG.HorizontalTo SVG.OriginRelative (dx:dxs):ds)
            = DLineTo ap : renderPathCommands firstp ap Nothing (cont dxs)
            where
                ap = (cx+dx,cy)

                cont [] = ds
                cont dxs' = SVG.HorizontalTo SVG.OriginRelative dxs' : ds

        renderPathCommands firstp (cx, _) _ (SVG.VerticalTo SVG.OriginAbsolute (py:pys):ds)
            = DLineTo ap : renderPathCommands firstp ap Nothing (cont pys)
            where
                ap = (cx,py)

                cont [] = ds
                cont pys' = SVG.VerticalTo SVG.OriginAbsolute pys' : ds

        renderPathCommands firstp (cx, cy) _ (SVG.VerticalTo SVG.OriginRelative (dy:dys):ds)
            = DLineTo ap : renderPathCommands firstp ap Nothing (cont dys)
            where
                ap = (cx,cy+dy)

                cont [] = ds
                cont dys' = SVG.VerticalTo SVG.OriginRelative dys' : ds

        renderPathCommands firstp currentp _ (SVG.CurveTo origin ((c1,c2,p):ps):ds)
            = DBezierTo ac1 ac2 ap : renderPathCommands firstp ap (Just ac2) (cont ps)
            where
                ap = toAbsolute currentp origin (fromRPoint p)
                ac1 = toAbsolute currentp origin (fromRPoint c1)
                ac2 = toAbsolute currentp origin (fromRPoint c2)

                cont [] = ds
                cont ps' = SVG.CurveTo origin ps' : ds

        renderPathCommands firstp currentp mbControlp (SVG.SmoothCurveTo origin ((c2,p):ps):ds)
            = DBezierTo ac1 ac2 ap : renderPathCommands firstp ap (Just ac2) (cont ps)
            where
                ap = toAbsolute currentp origin (fromRPoint p)
                ac1 = maybe ac2 (mirrorControlPoint currentp) mbControlp
                ac2 = toAbsolute currentp origin (fromRPoint c2)

                cont [] = ds
                cont ps' = SVG.SmoothCurveTo origin ps' : ds

        renderPathCommands firstp currentp _ (SVG.QuadraticBezier origin ((c1,p):ps):ds)
            = cbezier : renderPathCommands firstp ap (Just ac1) (cont ps)
            where
                ap = toAbsolute currentp origin (fromRPoint p)
                ac1 = toAbsolute currentp origin (fromRPoint c1)

                cbezier = bezierQ2C currentp ac1 ap

                cont [] = ds
                cont ps' = SVG.QuadraticBezier origin ps' : ds

        renderPathCommands firstp currentp mbControlp (SVG.SmoothQuadraticBezierCurveTo origin (p:ps):ds)
            = cbezier : renderPathCommands firstp ap (Just ac1) (cont ps)
            where
                ap = toAbsolute currentp origin (fromRPoint p)
                ac1 = maybe currentp (mirrorControlPoint currentp) mbControlp

                cbezier = bezierQ2C currentp ac1 ap

                cont [] = ds
                cont ps' = SVG.SmoothQuadraticBezierCurveTo origin ps' : ds

        renderPathCommands firstp currentp _ (SVG.EllipticalArc origin ((rx,ry,rot,largeArcFlag,sweepFlag,p):ps):ds)
            = convertSvgArc currentp rx ry rot largeArcFlag sweepFlag ap ++ renderPathCommands firstp ap Nothing (cont ps)
            where
                ap = toAbsolute currentp origin (fromRPoint p)

                cont [] = ds
                cont ps' = SVG.EllipticalArc origin ps' : ds

        renderPathCommands firstp@(fx,fy) (cx,cy) mbControlp (SVG.EndPath:ds)
            | fx /= cx || fy /= cy
                = DLineTo firstp : renderPathCommands firstp firstp mbControlp ds
            | otherwise
                = renderPathCommands firstp firstp mbControlp ds

        renderPathCommands _ _ _ _ = []

        renderTree :: TransformationMatrix -> SVG.Tree -> [DrawOp]
        renderTree m (SVG.GroupTree g) = renderTrees (applyTransformations m (SVG._transform (SVG._groupDrawAttributes g))) (SVG._groupChildren g)
        renderTree m (SVG.PathTree p) = map (transformDrawOp tr) $ renderPathCommands (0,0) (0,0) Nothing (SVG._pathDefinition p)
           where
                tr = applyTransformations m (SVG._transform (SVG._pathDrawAttributes p))

        renderTree m (SVG.RectangleTree r)
            | rx == 0.0 && ry == 0.0
                = map (transformDrawOp tr) [DMoveTo (x,y), DLineTo (x+w,y), DLineTo (x+w,y+h), DLineTo (x,y+h), DLineTo (x,y)]
            | otherwise
                = map (transformDrawOp tr)
                      ([DMoveTo (x,y+ry)]     ++ convertSvgArc (x,y+ry) rx ry 0 False True (x+rx, y) ++
                       [DLineTo (x+w-rx,y)]   ++ convertSvgArc (x+w-rx,y) rx ry 0 False True (x+w, y+ry) ++
                       [DLineTo (x+w,y+h-ry)] ++ convertSvgArc (x+w,y+h-ry) rx ry 0 False True (x+w-rx, y+h) ++
                       [DLineTo (x+rx,y+h)]   ++ convertSvgArc (x+rx, y+h) rx ry 0 False True (x, y+h-ry) ++
                       [DLineTo (x,y+ry)])
            where
                (x,y) = fromSvgPoint dpi (SVG._rectUpperLeftCorner r)
                w = fromSvgNumber dpi (SVG._rectWidth r)
                h = fromSvgNumber dpi (SVG._rectHeight r)
                (rx, ry) = mapTuple (fromSvgNumber dpi) (SVG._rectCornerRadius r)
                tr = applyTransformations m (SVG._transform (SVG._rectDrawAttributes r))

        renderTree m (SVG.LineTree l) = [DMoveTo p1, DLineTo p2]
            where
                p1 = transformPoint tr (fromSvgPoint dpi (SVG._linePoint1 l))
                p2 = transformPoint tr (fromSvgPoint dpi (SVG._linePoint2 l))
                tr = applyTransformations m (SVG._transform (SVG._lineDrawAttributes l))

        renderTree m (SVG.PolyLineTree l) = map (transformDrawOp tr) (DMoveTo p0:map DLineTo ps)
            where
                (p0:ps) = map (\(Linear.V2 x y) -> (x,y)) (SVG._polyLinePoints l)
                tr = applyTransformations m (SVG._transform (SVG._polyLineDrawAttributes l))

        renderTree m (SVG.PolygonTree l) = map (transformDrawOp tr) (DMoveTo p0:map DLineTo (ps ++ [p0]))
            where
                (p0:ps) = map (\(Linear.V2 x y) -> (x,y)) (SVG._polygonPoints l)
                tr = applyTransformations m (SVG._transform (SVG._polygonDrawAttributes l))

        renderTree m (SVG.EllipseTree e) = map (transformDrawOp tr) (DMoveTo (cx-rx,cy) : bs1++bs2++bs3++bs4)
            where
                bs1 = convertSvgArc (cx-rx, cy) rx ry 0 False True (cx, cy-ry)
                bs2 = convertSvgArc (cx, cy-ry) rx ry 0 False True (cx+rx, cy)
                bs3 = convertSvgArc (cx+rx, cy) rx ry 0 False True (cx, cy+ry)
                bs4 = convertSvgArc (cx, cy+ry) rx ry 0 False True (cx-rx, cy)

                (cx,cy) = fromSvgPoint dpi (SVG._ellipseCenter e)
                rx = fromSvgNumber dpi (SVG._ellipseXRadius e)
                ry = fromSvgNumber dpi (SVG._ellipseYRadius e)
                tr = applyTransformations m (SVG._transform (SVG._ellipseDrawAttributes e))

        renderTree m (SVG.CircleTree c) = map (transformDrawOp tr) (DMoveTo (cx-r,cy) : bs1++bs2++bs3++bs4)
            where
                bs1 = convertSvgArc (cx-r, cy) r r 0 False True (cx, cy-r)
                bs2 = convertSvgArc (cx, cy-r) r r 0 False True (cx+r, cy)
                bs3 = convertSvgArc (cx+r, cy) r r 0 False True (cx, cy+r)
                bs4 = convertSvgArc (cx, cy+r) r r 0 False True (cx-r, cy)

                (cx,cy) = fromSvgPoint dpi (SVG._circleCenter c)
                r = fromSvgNumber dpi (SVG._circleRadius c)
                tr = applyTransformations m (SVG._transform (SVG._circleDrawAttributes c))

        {- The rest: None, UseTree, SymbolTree, TextTree, ImageTree -}
        renderTree _ _ = []

        renderTrees :: TransformationMatrix -> [SVG.Tree] -> [DrawOp]
        renderTrees m = concatMap (renderTree m)
