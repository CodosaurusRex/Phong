module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.Serialize
import Data.Either.Unwrap
import System.ZMQ
import Data.ByteString.Char8 hiding (putStrLn, getLine, any)
import System.IO
import Control.Monad
import Data.Word
import PhongCommon
import System.Environment
import System.Time
import Control.Concurrent.STM


main = 	withContext 1 $ \context -> do
  (ip:name:_) <- getArgs
  extraRequests <- any (== "--use-extra-requests") `liftM` getArgs 
  t0 <- getClockTime
  t0' <- newTVarIO t0 
  putStrLn' "Connecting to Pong server..."  
  let which = case name of
        "9111" -> Right ()
        "7201" -> Left ()
  withSocket context Req $ \socket -> do
    connect socket ("tcp://" ++ ip ++ ":" ++ name)
    putStrLn' "Connected"
    init <- initWorld socket
    playIO (InWindow "Pong" (1000, 1000) (10,10)) white 10 (init) (makePic socket)(moveit t0' socket which) (stepWorld socket extraRequests)
  			    

reqStateUp :: Socket Req -> IO World
reqStateUp socket = do
	putStrLn' "About to request from stateupdate"
	send socket (encode (StateUp)) []
	putStrLn' "sent stateupdate request"
	reply <-receive socket []
	putStrLn' "received stateupdate reply from server"
        --print (decode reply :: Either String World)
	case decode reply of 
	     Right w -> return w
	     Left s -> error ("gtfo" ++ s)
             
             
reqMove :: Socket Req -> Request -> IO World
reqMove socket pos = do
  putStrLn' "move requested"
  send socket (encode (pos)) []
  reply <-receive socket []
  {-case decode reply of 
    Right w -> return w
    Left s -> error ("gtfo" ++ s)-}
  reqStateUp socket


initWorld :: Socket Req-> IO World
initWorld socket= reqStateUp socket

makePic :: Socket Req -> World -> IO Picture
makePic socket _ = drawit `liftM` (reqStateUp socket) 

drawit :: World -> Picture
drawit (World b p1 p2 _) = Pictures [(drawB b), (drawp p1), (drawp p2)]

drawB :: Ball -> Picture
drawB (Ball (x,y) _) = Translate x y $ Circle 50

drawp :: Player -> Picture
drawp (Player (x,y) _) = Translate x y $ polygon (rectanglePath 20 100)

moveit :: TVar ClockTime -> Socket Req-> WhichPaddle->Event -> World -> IO World
moveit t0' s which (EventMotion(x, y)) w =  do
  t1 <- getClockTime
  dt <- atomically $ do
        t0 <- readTVar t0'
        writeTVar t0' t1
        return $ diffClockTimes t1 t0
  if dt > (TimeDiff 0 0 0 0 0 0 10000000000) -- 500 milliseconds
    then reqMove s (PosUpdate which (x,y))
    else return w
moveit _ s which a w = return w -- Ignore all except mouse motion events

stepWorld :: Socket Req -> Bool -> Float -> World -> IO World
stepWorld s erqs f w = case erqs of
  False -> return w
  True -> do
    send s (encode StateUp) []
    r <- receive s []
    return $ seq r w
    