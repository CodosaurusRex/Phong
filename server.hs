import System.ZMQ
import Control.Monad (forever)
import Data.ByteString
import Control.Concurrent (threadDelay)
import Data.Serialize
import Graphics.Gloss

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


main :: IO ()
main = withContext 1 $ \context -> do
    Prelude.putStrLn "Connecting to Clients..."
   -- withSocket context Rep $ \left -> do
       -- connect left "tcp://localhost:1618"
    withSocket context Rep $ \rightp -> do
            bind rightp "tcp://*:3141"
	    Prelude.putStrLn "Connected."
	    message <- receive rightp []
	    send rightp (encode (initi))[]
            --forever $ do
               -- putStrLn 

initi :: World
initi = World (Ball (0,0) (10,0)) (Player (-200,0) (0,0)) (Player (200,0)(0,0))