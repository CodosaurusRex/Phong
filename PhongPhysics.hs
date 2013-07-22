module PhongPhysics where

import PhongCommon
{-
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
    

--paddleCollideTime :: GameBoard -> 
-}

collidePaddle :: Ball -> Player -> Player -> Ball
collidePaddle b@(Ball (x, y) (vx, vy) (s1, s2)) (Player (px, py) _) (Player (px2, py2) _)
  |((x-5) < (px+5)) && (y > (py-50)) && (y < (py + 50)) = Ball (x, y) (-vx*(4/3), vy+ (10* (y-py))) (s1, s2)
  |((x+5) > (px2-5)) && (y > (py2-50)) && (y < (py2 + 50)) = Ball (x, y) (-vx*(9/8), (vy + (10*(y-py2)))) (s1, s2)
  |otherwise = collideWall b
               

collideWall :: Ball -> Ball
collideWall b@(Ball (x,y) (vx, vy) (s1, s2))
  |y > 495 || y < -225 = (Ball (x,y) (vx, -vy) (s1, s2))
  |vx < 0 = Ball (x, y) (vx + 0.005, vy) (s1, s2)
  |otherwise = checkScore b
   

checkScore:: Ball -> Ball
checkScore  (Ball (x,y) (vx, vy) (s1, s2))
  |x > 500 = (Ball (0,0) (-120,0) (s1, s2+1))
  |x < -500 = (Ball (0,0) (120,0) (s1+1, s2))
  |otherwise = Ball (x, y) (vx, vy) (s1, s2)