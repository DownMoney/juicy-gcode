module Types
  ( Point,
    DrawOp (..),
    if',
    MachineSettings (..),
  )
where

import Data.GCode.Types ( GCode ) 


type Point = (Double, Double) -- A point in the plane, absolute coordinates

-- all of them are invariant under affine transformation
data DrawOp
  = DMoveTo Point
  | DLineTo Point -- End point
  | DBezierTo Point Point Point -- Control point1, control point2, end point
  deriving (Show)

-- just to make it available everywhere
if' :: Bool -> t -> t -> t
if' True t _ = t
if' False _ f = f

data MachineSettings = MachineSettings
  { _begin :: GCode,
    _end :: GCode,
    _toolon :: GCode,
    _tooloff :: GCode
  }
