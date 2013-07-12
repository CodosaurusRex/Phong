module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.Serialize
import Data.Either.Unwrap
import System.ZMQ
import Data.ByteString.Char8 hiding (putStrLn)
import System.IO
import Control.Monad
import Data.Word

data Request = PosUpdate (Point) | StateUp

data Player = Player Point
     	      	     Vector --movement 
		     	    	       deriving (Show, Read)
data Ball = Ball 
     	    {  point::  (Float, Float) --place in space
     	      ,vector:: (Float, Float) --movement Vector
	    } 
					 deriving (Show, Read)
data World = World
     	   { ball :: Ball
	    ,player1 :: Player
	    ,player2 :: Player
	   } deriving (Show, Read)

instance Serialize World where
	 put (World b p1 p2)   = do put b
                                    put p1
                                    put p2
	 get		 = 	 do  b <- get
	    	    	  	     p1 <- get
				     p2 <- get
				     return (World b p1 p2) 

instance Serialize Ball where
	 put (Ball (x,y) (a,b)) = do  put (x,y)
	     	   	 	      put (a,b)
	 get = 			do (x,y) <- get
	    	    	  	   (a,b) <- get
				   return (Ball (x,y) (a,b))

instance Serialize Player where
	 put (Player (x,y) (a,b)) = do put (x,y)
	     	     	   	       put (a, b)
	 get = 			    do (x,y) <- get
	    	    	  	       (a,b) <- get
				       return (Player (x,y) (a,b))	  

instance Serialize Request where
	 put (PosUpdate p)	= do (put (0 :: Word8))
	     			     (put p)

	 put (StateUp)		= put (1 :: Word8)
	 
	 get = do t <- get :: Get Word8
                  case t of
                     0 -> do p <- get
                             return (PosUpdate p)
                     1 -> return (StateUp)

main = do
	withContext 1 $ \context -> do  
     		 System.IO.putStrLn "Connecting to Pong server..."  
  		 withSocket context Req $ \socket -> do
    		 	    connect socket "tcp://localhost:3141"
			    Prelude.putStrLn "Connected"
			    init <- initWorld socket
       			    --playIO (InWindow "Pong" (1000, 1000) (10,10)) black 10 (init) (makePic socket)(moveit socket) (stepWorld socket)
			    putStrLn (show init)
			    

reqStateUp :: Socket Req -> IO World
reqStateUp socket = do
	putStrLn "About to send"
	send socket (encode (StateUp)) []
	putStrLn "sent"
	reply <-receive socket []
	putStrLn "received transmission"
	case decode reply of 
	     Right w -> return w
	     Left s -> error ("gtfo" ++ s)
reqMove :: Socket Req -> Point -> IO World
reqMove socket pos = do
	send socket (encode ()) []
	reply <-receive socket []
	case decode reply of 
	     Right w -> return w
	     Left s -> error ("gtfo" ++ s)


initWorld :: Socket Req-> IO World
initWorld socket= reqStateUp socket

makePic :: Socket Req -> World -> IO Picture
makePic socket _ = drawit `liftM` (reqStateUp socket) 

drawit :: World -> Picture
drawit (World b p1 p2) = Pictures [(drawB b), (drawp p1), (drawp p2)]

drawB :: Ball -> Picture
drawB (Ball (x,y) _) = Translate x y $ Circle 50

drawp :: Player -> Picture
drawp (Player (x,y) _) = Translate x y $ polygon (rectanglePath 20 100)

moveit :: Socket Req-> Event-> World -> IO World
moveit s (EventMotion(x, y)) _ =  reqMove s (x,y)

stepWorld :: Socket Req-> Float -> World -> IO World
stepWorld s f w = return w  

