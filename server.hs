import System.ZMQ
import Control.Monad (forever)
import Data.ByteString hiding (putStrLn')
import Control.Concurrent (threadDelay)
import Data.Serialize
import Graphics.Gloss
import Data.Either.Unwrap
import PhongCommon
import Control.Concurrent (threadDelay, forkIO)
import Data.Serialize
import Graphics.Gloss
import Control.Concurrent.STM


main :: IO ()
main = withContext 1 $ \context -> do
  {-Prelude.putStrLn' "Connecting to Clients..."
  withSocket context Rep $ \leftp -> do
    bind leftp "tcp://*:7001"
    withSocket context Rep $ \rightp -> do
      bind rightp "tcp://*:7000"
      Prelude.putStrLn' "Connected."-}
      myWorld <- atomically $ newTVar initi
      putStrLn' "Connecting to Clients..."
      withSocket context Rep $ \leftp -> do
        bind leftp "tcp://*:7201"
        withSocket context Rep $ \rightp -> do
          bind rightp "tcp://*:9111"
          putStrLn' "Bound."
          -- message <- receive rightp [] -- This was a test line.  Doesn't stay in full program
          -- send rightp (encode (initi))[]
          putStrLn' "Done initializing."
          forkIO $ runThroughTime 0.5 myWorld
          -- Poll for messages from leftp and rightp
          forever $ do
            (poll [S rightp In, S leftp In] (-1) >>= mapM_ (\(S s _) -> handleSocket s myWorld))           
            -- putStrLn' 


handleSocket :: Socket a ->TVar World-> IO()
handleSocket s w = do
  putStrLn' "handle"			
  inp <- receive s []
  putStrLn' "Yo this thing is working soooo"
  let a = fromRight $ decode(inp)
  case decode(inp) of
    Right (PosUpdate p (x,y)) ->  do
                          movePaddle a w 
                          send s (encode "") []
    Right (StateUp) -> do
      world <- atomically $ readTVar w
      send s (encode(world))[] 	     
    Left b -> do 
      print b                           
      print $ inp
	
initi :: World
initi = World (Ball (0,0) (10,0)) (Player (-200,0) 0) (Player (200,0) 0) True


-- Placeholder for real world-stepping
stepWorld :: Float -> World -> World
stepWorld dt w@(World b@(Ball (x,y) v) _ _ _ ) = w { ball = Ball (x+dt,y+dt) v }

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
                          w {player1 = p1 {padpoint= (x, y)}}
                        Left () -> 
                          w {player2 = p2 {padpoint=  (x, y)}}
                  writeTVar wt newWorld
                                                    
