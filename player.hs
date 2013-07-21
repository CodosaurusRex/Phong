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
  playIO (InWindow "Pong" (1000, 1000) (10,10)) 
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
--    Right Nothing  -> do
--      putStrLn' "Requested state from server, got nothing."
--      return initi
    Left s -> do 
      putStrLn' $ "Bad decode on response to reqStateUp" ++ s ++ " : " ++ show reply'
      return initi
             
             
{-
reqMove :: TVar Request -> Request -> TVar Request -> IO World
reqMove h pos = do
  putStrLn' "move requested"
  hPutStrLn h (encode (pos))
  reply <- hGetLine h
  case (decode reply :: Either String (Maybe World)) of
    Right Nothing -> do
      putStrLn' "Expected Nothing from server, got Nothing"
      return initi
    Right (Just w) -> do
      putStrLn' $ "Expected Nothing from server, got world " ++ show w
      return initi
    Left _ -> do 
      putStrLn' "Bad decode on server's response to move request"
      return initi
--  seq reply (reqStateUp h)
-}

initWorld :: Handle -> TVar Request -> MVar () -> IO World
initWorld = reqStateUp

makePic :: Handle -> TVar Request -> MVar () -> World -> IO Picture
makePic h nextPosRequest' hSync _ =  drawit `liftM` (reqStateUp h nextPosRequest' hSync) 
--makePic h hSync _ = drawit `liftM` (reqStateUp h) 

drawit :: World -> Picture
drawit (World b p1 p2 _) = Pictures [(drawB b), (drawp p1), (drawp p2)]

drawB :: Ball -> Picture
drawB (Ball (x,y) _) = Color white $ Translate x y $ circleSolid 10

drawp :: Player -> Picture
drawp (Player (x,y) _) = Color white $Translate x y $ polygon (rectanglePath 20 100)

moveit :: TVar Request -> WhichPaddle -> Event -> World -> IO World
moveit nextPosRequest' which (EventMotion(x, y)) w = do
  atomically $ writeTVar nextPosRequest' (PosUpdate which (x,y))
  return w
moveit _ _ _ w = return w -- Ignore all except mouse motion events

stepWorld :: Float -> World -> IO World
stepWorld _ w = return w
--stepWorld _ hSync _ w = return w

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
