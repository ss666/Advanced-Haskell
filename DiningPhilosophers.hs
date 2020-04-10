import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import System.Random

type Fork =TVar Bool
type Record =TChan String

philosophers:: [String]
philosophers = ["A","B","C","D","E"]

putFork ::Fork -> Fork -> STM()
putFork left right = do
    writeTVar left True
    writeTVar right True

takeFork ::Fork -> Fork -> STM()
takeFork left right = do
    leftAvailable <- readTVar left
    rightAvailable <- readTVar right
    if leftAvailable && rightAvailable
        then do writeTVar left False 
                writeTVar right False
        else retry   

run :: String ->Fork -> Fork ->Record -> IO()
run philosopher left right record = forever $ do   
     atomically $ writeTChan record (philosopher ++ " is going to eat.")
     atomically $ takeFork left right
     atomically $ writeTChan record (philosopher ++ " got forks") 
     delay <-randomRIO(1,10)
     threadDelay (delay * 1000000)
     atomically $ putFork left right
     atomically $ writeTChan record (philosopher ++ " drops forks and begins thinking")
     delay <-randomRIO(1,10)
     threadDelay (delay  * 1000000)

main :: IO()
main = do
    record <- newTChanIO
    forks <- replicateM 5 $ newTVarIO True
    forM_ [0..4] $ \i ->
        do let left = forks !! i
               right = forks !! ((i+1) `mod` 5)
               philosopher = philosophers !! i
            in forkIO $ forever $ run philosopher left right record
    forever $ do 
     result <- atomically $ readTChan record
     putStrLn result