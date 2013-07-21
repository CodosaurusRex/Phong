import Control.Monad (forever)
import Data.ByteString hiding (putStrLn, hGetLine, hPutStrLn, hGetLine)
import Data.ByteString.Char8
import Control.Concurrent (threadDelay)
import Data.Serialize
import Graphics.Gloss
import Data.Either.Unwrap
import PhongCommon
import Control.Concurrent (threadDelay, forkIO)
import Data.Serialize
import Graphics.Gloss
import Control.Concurrent.STM
import Graphics.Gloss.Data.Vector
import Network
import Control.Concurrent.MVar
import GHC.IO.Handle hiding (hGetLine)

talkWith :: Handle -> TVar World -> IO ()
talkWith h w' = do
  putStrLn' $ "talking with " ++ show h
  hSetBuffering h NoBuffering
  forever $ do
    putStrLn' $ show h ++ ": getWithSize"
    req' <- getWithSize h
    putStrLn' $ "Got line: " ++ show req'
    case req' of
      Right req -> do resp <- handleRequest req w'
                      putStrLn' $ "About to send encode of: " ++ show resp
                      sendWithSize h (encode resp)
                      putStrLn' $ "Its encoding is: " ++ show (encode resp)
                      let testDecode = decode (encode resp) :: Either String World
                      putStrLn' $ "Its encode decodes to: " ++ show testDecode
      Left _    -> error $ "Bad request: " ++ show req'
      

summarizeHandle :: Handle -> IO ()
summarizeHandle h = do
  putStrLn' $ "Handle: " ++ show h
  bf <- hGetBuffering h
  putStrLn' $ "Buffering: " ++ show bf
  en <- hGetEncoding h
  putStrLn' $ "Encoding: " ++ show en

-- |Update or just retrieve world according to request
handleRequest :: Request -> TVar World -> IO World
handleRequest r@(PosUpdate _ _)  w' = do
  movePaddle r w' 
  readTVarIO w'
handleRequest ToggleRunning w' = atomically $ do
  w <- readTVar w'
  writeTVar w' (w {isRunning = not(isRunning w)})
  return w
handleRequest StateUp w'            = do
  w <- atomically $ readTVar w'
  error "Warning, got StateUp, but we don't expect to see this constructor anymore."
  return w
  
main :: IO ()
main = do
  myWorld <- atomically $ newTVar initi
  forkIO $ runThroughTime 0.01 myWorld
  putStrLn' "Accepting to Clients..."
  sock <- listenOn (PortNumber pongPort)
  forever $ do
    (handle, host, port) <- accept sock
    Prelude.putStrLn $ Prelude.unwords 
      ["Got connection from ",show handle, show host, show port]
    syncMV <- newMVar ()
    forkIO $ talkWith handle myWorld

-- Placeholder for real world-stepping
stepWorld :: Float -> World -> World
stepWorld dt w@(World b@(Ball (x,y) (vx, vy)) _ _ _ ) = w { ball = Ball ((x + (dt * vx)), (y + (dt*vy))) ((vx- 0.01),(vy- 0.01)) }

runThroughTime :: Float -> TVar World -> IO ()
runThroughTime dt worldT = forever $ do 
--  putStrLn' "time steppp"
  threadDelay $ floor (dt * 1000000)
  atomically $ do
    w <- readTVar worldT
    writeTVar worldT $ stepWorld dt w

movePaddle :: Request -> TVar World -> IO ()
movePaddle (PosUpdate p (x,y)) wt = 
  atomically $ do w@(World b p1 p2 r)<- readTVar wt
                  let newWorld = case p of
                        Right () -> 
                          w {player1 = p1 {padpoint= (-500, y)}}
                        Left () -> 
                          w {player2 = p2 {padpoint=  (500, y)}}
                  writeTVar wt newWorld


{-
-- |ZMQ-based req socket loop body
handleSocket :: Socket a ->TVar World-> IO()
handleSocket s w = do
  putStrLn' "handle"			
  inp <- receive s []
  seq inp (putStrLn' "Yo this thing is working soooo")
  let a = fromRight $ decode(inp)
  case decode(inp) of
    Right (PosUpdate p (x,y)) ->  do
                          movePaddle a w 
                          send s (encode "") []
    Right (StateUp) -> do
      world <- atomically $ readTVar w
      send s (encode(world))[] 	     
    Left b -> do 
      print "Left decode from server"
      return ()
-}	
