module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.Serialize
import Data.Either.Unwrap
import Data.ByteString.Char8 hiding (putStrLn, getLine, any)
import System.IO hiding (hPutStrLn, hGetLine)
import Control.Monad
import Data.Word
import PhongCommon
import System.Environment
import System.Time
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Network

main :: IO ()
main = do	
  (ip:paddlePref:_) <- getArgs
  t0 <- getClockTime
  t0' <- newTVarIO t0 
  putStrLn' "Connecting to Pong server..."  
  let paddleSide = case paddlePref of
        "Right" -> Right ()
        "Left" -> Left ()
  h <- connectTo ip (PortNumber pongPort)
  putStrLn' "Connected"
  initW <- initWorld h
  playIO (InWindow "Pong" (1000, 1000) (10,10)) black 10 (initW) (makePic h)(moveit t0' h paddleSide) (stepWorld h)
  			    

reqStateUp :: Handle -> IO World
reqStateUp h = do
	putStrLn' "About to request from stateupdate"
	hPutStrLn h (encode (StateUp))
	putStrLn' "sent stateupdate request"
	reply <- hGetLine h
	putStrLn' "received stateupdate reply from server"
        --print (decode reply :: Either String World)
	case decode reply of 
	     Right w -> return w
	     Left s -> error ("gtfo" ++ s)
             
             
reqMove :: Handle -> Request -> IO World
reqMove h pos = do
  putStrLn' "move requested"
  hPutStrLn h (encode (pos))
  reply <- hGetLine h
  seq reply (reqStateUp h)


initWorld :: Handle -> IO World
initWorld socket= reqStateUp socket

makePic :: Handle -> World -> IO Picture
makePic socket _ = drawit `liftM` (reqStateUp socket) 

drawit :: World -> Picture
drawit (World b p1 p2 _) = Pictures [(drawB b), (drawp p1), (drawp p2)]

drawB :: Ball -> Picture
drawB (Ball (x,y) _) = Color white $ Translate x y $ circleSolid 10

drawp :: Player -> Picture
drawp (Player (x,y) _) = Color white $Translate x y $ polygon (rectanglePath 20 100)

moveit :: TVar ClockTime -> Handle -> WhichPaddle->Event -> World -> IO World
moveit t0' h which (EventMotion(x, y)) w =  do
  t1 <- getClockTime
  dt <- atomically $ do
        t0 <- readTVar t0'
        writeTVar t0' t1
        return $ diffClockTimes t1 t0
  if dt > (TimeDiff 0 0 0 0 0 0 10000000000) -- 500 milliseconds
    then reqMove h (PosUpdate which (x,y))
    else return w
moveit _ s which a w = return w -- Ignore all except mouse motion events

stepWorld :: Handle -> Float -> World -> IO World
stepWorld _ _ w = return w

{- 
stepWorld :: Socket Req -> Bool -> Float -> World -> IO World
stepWorld s erqs f w = case erqs of
  False -> return w
  True -> do
    send s (encode StateUp) []
    r <- receive s []
    putStrLn "Sent extra request"
    print r
    return $ seq r w
-} 
