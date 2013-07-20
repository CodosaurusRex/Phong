import Control.Monad (forever)
import Data.ByteString hiding (putStrLn, getLine)
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
import GHC.IO.Handle

talkWith :: Handle -> MVar () -> TVar World -> IO ()
talkWith h syncMV w' = do
  hSetBuffering h LineBuffering
  forever $ do
    _ <- takeMVar syncMV
    line <- getLine h
    resp <- handleRequest (read h) w'
    putMVar syncMV ()

handleRequest :: Request -> TVar World -> IO (Maybe World)
handleRequest r@(PosUpdate _ _)  w' = movePaddle r w' >> return Nothing
handleRequest StateUp w'            = do
  w <- atomically $ readTVar w'
  return $ Just w
  
main :: IO ()
main = do
  myWorld <- atomically $ newTVar initi
  putStrLn' "Accepting to Clients..."
  sock <- listenOn (PortNumber pongPort)
  forever $ do
    (handle, host, port) <- accept sock
    putStrLn $
      unwords ["Got connection from ",show handle, show host, show port]
    syncMV <- newMVar ()
    forkIO $ talkWith handle syncMV

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
	
initi :: World
initi = World (Ball (0,0) (0.1,0)) (Player (-500,0) 0) (Player (500,0) 0) True


-- Placeholder for real world-stepping
stepWorld :: Float -> World -> World
stepWorld dt w@(World b@(Ball (x,y) (vx, vy)) _ _ _ ) = w { ball = Ball ((x + (dt * vx)), (y + (dt*vy))) ((vx-1),(vy-1)) }

runThroughTime :: Float -> TVar World -> IO ()
runThroughTime dt worldT = forever $ do 
  putStrLn' "time steppp"
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

