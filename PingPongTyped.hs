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

$( derive makeBinary ''PingMessage )
$( derive makeBinary ''PongMessage )

pingServer :: ReceivePort PingMessage -> Process ()
pingServer rcvPort = do
    (Ping fromPort) <- receiveChan rcvPort
    liftIO . putStrLn $ "Got ping from: " ++ show fromPort
    sendChan fromPort Pong
    pingServer rcvPort

pingClient :: SendPort PingMessage -> Process ()
pingClient serverSPort = do
    (sendPort, rcvPort) <- newChan
    sendChan serverSPort (Ping sendPort)
    Pong <- receiveChan rcvPort
    
    liftIO . putStrLn $ "Got pong from server"
    liftIO $ threadDelay (1*1000000)
    pingClient serverSPort

remotable ['pingClient]

master :: Backend -> [NodeId] -> Process ()
master backend slaves = do

    node <- getSelfNode

    (sendPort :: SendPort PingMessage, rcvPort :: ReceivePort PingMessage) <- newChan

    forM_ slaves $ \slave -> spawn slave $ $(mkClosure 'pingClient) (sendPort :: SendPort PingMessage)

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

