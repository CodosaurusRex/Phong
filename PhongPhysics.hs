module PhongPhysics where

import PhongCommon

floorOrCeilingCollideTime :: GameBoard -> World -> Float -> Maybe Float
floorOrCeilingCollideTime (GameBoard rmH rmW (rmX0,rmY0) _ _ ballR) 
  (World (Ball (x,y) (vx,vy)) _ _ _) dt =
  if collideTime < dt then Just collideTime else Nothing
  where
    collideTarget 
      | vy == 0  = Nothing
      | vy >  0  = rmH - rmY0
      | vy <  0  = rmX0
    collideTime = fmap (\targetY -> (abs (targetY - y) - ballR) / vy) collideTarget
    

paddleCollideTime :: GameBoard -> 