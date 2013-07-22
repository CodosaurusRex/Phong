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
import Control.Concurrent.STM.TChan
import Control.Concurrent.MVar
import Network


main :: IO ()
main = do	
  (ip:paddlePref:_) <- getArgs
  nextPosRequest' <- newTVarIO $ dummyRequest
  hSync <- newMVar ()
  putStrLn' "Connecting to Pong server..."  
  let paddleSide = case paddlePref of
        "Right" -> Right ()
        "Left" -> Left ()
  h <- connectTo ip (PortNumber pongPort)
  putStrLn' "Connected"
  initW <- initWorld h nextPosRequest' hSync
  playIO (InWindow "Pong" (1000,1000) (10,10)) 
    black 10 (initW) (makePic h nextPosRequest' hSync) 
    (moveit nextPosRequest' paddleSide) (stepWorld)
  			    
dummyRequest :: Request
dummyRequest = PosUpdate (Right ()) (0,0)

reqStateUp :: Handle -> TVar Request -> MVar () -> IO World
reqStateUp h nextPosRequest' hSync = withMVar hSync $ \_ -> do
  nextPosRequest <- readTVarIO nextPosRequest'
  putStrLn' $ "About to request from stateupdate: " ++ show nextPosRequest
  sendWithSize h nextPosRequest
  putStrLn' $ "sent stateupdate request"
  _ <- hGet h 8  -- HACK: WHY DO I HAVE TO DO THIS? 
  reply' <- getWithSize h
  putStrLn' $ "got line: " ++ show reply'
  case reply' of 
    Right reply -> return reply
    Left s -> do 
      putStrLn' $ "Bad decode on response to reqStateUp" ++ s ++ " : " ++ show reply'
      return initi

initWorld :: Handle -> TVar Request -> MVar () -> IO World
initWorld = reqStateUp

makePic :: Handle -> TVar Request -> MVar () -> World -> IO Picture
makePic h nextPosRequest' hSync _ =  drawit `liftM` (reqStateUp h nextPosRequest' hSync) 

drawit :: World -> Picture
drawit (World b p1 p2 _) = Pictures [(drawB b), (drawp p1), (drawp p2)]

drawB :: Ball -> Picture
drawB (Ball (x,y) _ (s1, s2)) = Pictures [Color white $ Translate x y $ circleSolid 10, Translate (-100) 485 $ Scale 0.1 0.1 $ Color white $ Text ("Score 1: " ++ (show s1)), Translate 0 485 $ Scale 0.1 0.1 $ Color white $ Text ("Score 2: " ++ (show s2))]

drawp :: Player -> Picture
drawp (Player (x,y) _) = Color white $Translate x y $ polygon (rectanglePath 20 100)

moveit :: TVar Request -> WhichPaddle -> Event -> World -> IO World
moveit nextPosRequest' which (EventMotion(x, y)) w = do
  atomically $ writeTVar nextPosRequest' (PosUpdate which (x,y))
  return w
moveit _ _ _ w = return w -- Ignore all except mouse motion events

stepWorld :: Float -> World -> IO World
stepWorld _ w = return w
