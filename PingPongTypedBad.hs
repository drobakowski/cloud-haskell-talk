{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, 
TupleSections, ScopedTypeVariables, TemplateHaskell, DeriveGeneric #-}

import Control.Concurrent (threadDelay)

import System.Environment (getArgs)
import Control.Distributed.Process
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Closure

import Control.Distributed.Process.Node hiding (newLocalNode)
import Control.Monad
import Data.Binary
import GHC.Generics (Generic)

import Data.Typeable
import Data.DeriveTH

data PongMessage = Pong
                 deriving (Generic, Typeable)

data PingMessage = Ping (SendPort PongMessage)
                 deriving (Generic, Typeable)

--instance Binary ChatProtocol
-- derive uses TH to generate the instance automatically
$( derive makeBinary ''PingMessage )
$( derive makeBinary ''PongMessage )

pingServer :: ReceivePort PingMessage -> Process ()
pingServer rcvPort = do
    (Ping fromPort) <- receiveChan rcvPort
    --Ping from <- expect
    liftIO . putStrLn $ "Got ping from: " ++ show fromPort
    sendChan fromPort Pong
    pingServer rcvPort

pingClient :: SendPort PongMessage -> Process ()
pingClient serverSPort = do
    liftIO . putStrLn $ "Got pong from server"
    (_sendPort, rcvPort) <- newChan
    --sendChan serverSPort (Ping sendPort)
    sendChan serverSPort Pong
    Pong <- receiveChan rcvPort
    
    liftIO . putStrLn $ "Got pong from server"
    liftIO $ threadDelay (1*1000000)
    pingClient serverSPort

remotable ['pingClient]

master :: Backend -> [NodeId] -> Process ()
master backend slaves = do

    node <- getSelfNode

    (sendPort :: SendPort PingMessage, rcvPort :: ReceivePort PingMessage) <- newChan

    -- BROKEN IMPLEMENTATION! => working progress
    -- It's the way mkClosure works, it forgets the type. It takes all Data with are !Serializable!
    -- Kein core dump oder crash!
    forM_ slaves $ \slave -> spawn slave $ $(mkClosure 'pingClient) (sendPort :: SendPort PingMessage)
    forM_ slaves $ \slave -> spawn slave $ $(mkClosure 'pingClient) (1 :: Integer)

    pingServer rcvPort

configSimpleLocalnetBackend :: String -> String -> IO Backend
configSimpleLocalnetBackend host port = initializeBackend host port $ __remoteTable initRemoteTable


main :: IO ()
main = do
    let host = "localhost"

    args <- getArgs

    case args of
        ["master", port] -> do
            backend <- configSimpleLocalnetBackend host port
            startMaster backend $ master backend  
        ["slave", port] -> do
            putStrLn $ "Starting slave on port: " ++ port
            backend <- configSimpleLocalnetBackend host port
            startSlave backend
        other -> do
            putStrLn $ "Unkown command: " ++ show other

