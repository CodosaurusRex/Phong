module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.Serialize
import Data.Either.Unwrap
import System.ZMQ
import Data.ByteString.Char8 hiding (putStrLn', getLine, putStrLn')
import System.IO
import Control.Monad
import Data.Word
import PhongCommon
import System.Environment


main = 	withContext 1 $ \context -> do
  (ip:name:_) <- getArgs
  putStrLn' "Connecting to Pong server..."  
  let which = case name of
        "9111" -> Right ()
        "7201" -> Left ()
  withSocket context Req $ \socket -> do
    connect socket ("tcp://" ++ ip ++ ":" ++ name)
    putStrLn' "Connected"
    init <- initWorld socket
    playIO (InWindow "Pong" (1000, 1000) (10,10)) white 10 (init) (makePic socket)(moveit socket which) (stepWorld socket)
  			    

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

moveit :: Socket Req-> WhichPaddle->Event -> World -> IO World
moveit s which (EventMotion(x, y)) _ =  reqMove s (PosUpdate which (x,y))
moveit s which a w = return w

stepWorld :: Socket Req-> Float -> World -> IO World
stepWorld s f w = return w  
