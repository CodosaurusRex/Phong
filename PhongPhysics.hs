module PhongPhysics where

import PhongCommon

floorOrCeilingCollideTime :: GameBoard -> World -> Float -> Maybe Float
floorOrCeilingCollideTime (GameBoard rmH rmW (rmX0,rmY0) _ _) 
  (World (Ball (x,y) (vx,vy)) _ _ _) dt =
  if collideTime < dt then Just collideTime else Nothing
  where
     = fmap min floorTime ceilTime
    