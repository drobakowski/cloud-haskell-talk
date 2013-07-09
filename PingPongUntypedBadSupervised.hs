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
import GHC.Generics (Generic)

import Data.Typeable
import Data.DeriveTH

data PingPongMessage = Ping ProcessId
                     | Pong ProcessId
                     deriving (Generic, Typeable)

$( derive makeBinary ''PingPongMessage )

pingServer :: Process ()
pingServer = forever $ do
    self <- getSelfPid
    liftIO . putStrLn $ "Starting Chat Server on port: " ++ show self
    Ping from <- expect
    liftIO . putStrLn $ "Got ping from: " ++ show from
    send from (Pong self)

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
    
    send serverPid (Pong self)

    liftIO . putStrLn $ "Got pong from: " ++ show serverPid
    liftIO $ threadDelay (1*1000000)
    pingClientBad serverPid

remotable ['pingServer, 'pingClient, 'pingClientBad]

master :: Backend -> [NodeId] -> Process ()
master backend slaves = do

    forever $ do
        node <- getSelfNode

        serverPid <- spawn node $ staticClosure $(mkStatic 'pingServer)

        _ <- monitor serverPid

        clients <- mapM (\slave -> spawn slave $ $(mkClosure 'pingClient) (serverPid)) slaves
        
        liftIO $ threadDelay (1*1000000)

        let badSlave = head slaves
        badClient <- spawn badSlave $ $(mkClosure 'pingClientBad) (serverPid)

        receiveWait [ match $ \(ProcessMonitorNotification _monitorRef pid reason) -> do
                                    liftIO . putStrLn $ "Process: " ++ (show pid) ++ " died with reason: " ++ (show reason)
                                    forM_ (badClient : clients) $ \cpid -> kill cpid "now"
                    ]

    --terminateAllSlaves backend

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

