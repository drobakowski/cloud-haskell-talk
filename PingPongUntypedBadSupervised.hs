{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, 
TupleSections, ScopedTypeVariables, TemplateHaskell, DeriveGeneric #-}

import System.IO
import Control.Concurrent (threadDelay)

import System.Environment (getArgs)
import Control.Distributed.Process
--import Control.Distributed.Process.Node
import Control.Distributed.Process.Node (initRemoteTable, runProcess)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Closure

--import System.Environment (getArgs)
--import Network.BSD(getHostName)
import Control.Distributed.Static (staticClosure)
--import Control.Distributed.Process
--import Control.Distributed.Process hiding (liftIO)
import Control.Distributed.Process.Serializable
--import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node hiding (newLocalNode)
--import Control.Distributed.Process.Backend.SimpleLocalnet
--import Control.Concurrent hiding (newChan)
--import qualified Control.Concurrent as C
import Control.Monad
--import Control.Monad.State hiding (liftIO)
--import Control.Applicative
--import Control.Monad.Trans hiding (liftIO)
--import Data.Binary hiding (get)
import Data.Binary
import GHC.Generics (Generic)

--import Data.Binary.Generic

--import Data.Monoid
import Data.Typeable
import Data.DeriveTH

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

--import Data.IORef
--import qualified Data.Set as S
--import qualified Data.Map as M

--terminateSlave :: NodeId -> Process ()
--printProcess s = liftIO $ putStrLn s

data PingPongMessage = Ping ProcessId
                     | Pong ProcessId
                     deriving (Generic, Typeable)

--instance Binary ChatProtocol
-- derive uses TH to generate the instance automatically
$( derive makeBinary ''PingPongMessage )

pingServer :: Process ()
pingServer = do
    self <- getSelfPid
    liftIO . putStrLn $ "Starting Chat Server on port: " ++ show self
    Ping from <- expect
    liftIO . putStrLn $ "Got ping from: " ++ show from
    send from (Pong self)
    pingServer

pingClient :: ProcessId -> Process ()
pingClient serverPid = do
    self <- getSelfPid

    send serverPid (Ping self)
    
    Pong serverPid <- expect
    liftIO . putStrLn $ "Got pong from: " ++ show serverPid
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

        monitor serverPid

        clients <- mapM (\slave -> spawn slave $ $(mkClosure 'pingClient) (serverPid)) slaves
        
        liftIO $ threadDelay (1*1000000)

        let badSlave = head slaves
        badClient <- spawn badSlave $ $(mkClosure 'pingClientBad) (serverPid)

        receiveWait [ match $ \(ProcessMonitorNotification monitorRef pid reason) -> do
                                    liftIO . putStrLn $ "Process: " ++ (show pid) ++ " died with reason: " ++ (show reason)
                                    forM_ (badClient : clients) $ \pid -> kill pid "now"
                    ]

    --terminateAllSlaves backend

configSimpleLocalnetBackend :: String -> String -> IO Backend
configSimpleLocalnetBackend host port = initializeBackend host port $ __remoteTable initRemoteTable


main :: IO ()
main = do
    let host = "localhost"
    --let host = getHostName

    --[port, serverType | _] <- getArgs

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


  --case a of True -> a
          --otherwise -> a

  --liftIO $ threadDelay (1*1000000)
  --return ()

  --newLocalNode

  --putStrLn spawnLocal

  --case args of
  -- ["master", host, port] -> do
  --   backend <- initializeBackend host port initRemoteTable 
  --   startMaster backend (master backend)
  -- ["slave", host, port] -> do
  --   backend <- initializeBackend host port initRemoteTable 
  --   startSlave backend

--main = do
--  -- Get some instructions
--  [serverType, port] <- getArgs
--  -- Set up the context
--   hostName <- getHostName
--   distributedContext <- initializeBackend hostName port (__remoteTable initRemoteTable)
--   -- The first thing a context lets you do is create a node.
--   node <- newLocalNode distributedContext
--   -- The other thing a context lets you do is find what other nodes are out there.
--   putStrLn "Discovering peers"
--   peers <- findPeers distributedContext 2000
--   putStrLn "Peers discovered"
--   -- About the only thing a node lets you do is run processes on it.
--   runProcess node (go serverType distributedContext peers)
