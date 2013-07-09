{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, 
TupleSections, ScopedTypeVariables, TemplateHaskell, DeriveGeneric #-}

import Control.Concurrent (threadDelay)

import System.Environment (getArgs)
import Control.Distributed.Process
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Closure

import Control.Distributed.Static (staticClosure)
import Control.Distributed.Process.Node hiding (newLocalNode)
import Control.Monad
import Data.Binary

import Data.Typeable

newtype PingMessage = Ping ProcessId deriving (Binary, Typeable)
newtype PongMessage = Pong ProcessId deriving (Binary, Typeable)

pingServer :: Process ()
pingServer = forever $

    receiveWait [
        match (\(Ping from) -> do
                    liftIO . putStrLn $ "Got ping from: " ++ show from
                    self <- getSelfPid
                    send from (Pong self)),
        match (\(Pong _from) -> liftIO . putStrLn $ "Got other msg"),
        matchUnknown $ liftIO . putStrLn $ "Unkown msg"
        ]

pingClient :: ProcessId -> Process ()
pingClient serverPid = do
    self <- getSelfPid

    send serverPid (Ping self)
    
    Pong from <- expect
    liftIO . putStrLn $ "Got pong from: " ++ show from
    liftIO $ threadDelay (1*1000000)
    pingClient serverPid

pingClientBad :: ProcessId -> Process ()
pingClientBad serverPid = do
    self <- getSelfPid
    
    liftIO . putStrLn $ "sending 'bad' message to server..."
    send serverPid (Pong self)

    liftIO $ threadDelay (1*1000000)
    pingClientBad serverPid

remotable ['pingServer, 'pingClient, 'pingClientBad]

master :: Backend -> [NodeId] -> Process ()
master backend slaves = do

    node <- getSelfNode

    serverPid <- spawn node $ staticClosure $(mkStatic 'pingServer)

    _ <- monitor serverPid

    forM_ slaves $ \slave -> spawn slave $ $(mkClosure 'pingClient) (serverPid)
    
    liftIO $ threadDelay (1*1000000)

    let badSlave = head slaves
    _ <- spawn badSlave $ $(mkClosure 'pingClientBad) (serverPid)

    receiveWait [ match $ \(ProcessMonitorNotification _monitorRef pid reason) -> liftIO . putStrLn $ "Process: " ++ (show pid) ++ " died with reason: " ++ (show reason) ]

    _ <- liftIO getLine
    terminateAllSlaves backend

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


