{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, 
TupleSections, ScopedTypeVariables, TemplateHaskell #-}

--import Control.Concurrent (threadDelay)

import System.Environment (getArgs)
import Control.Distributed.Process
--import Control.Distributed.Process.Node
import Control.Distributed.Process.Node (initRemoteTable, runProcess)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Closure

--import System.Environment (getArgs)
--import Network.BSD(getHostName)
--import Control.Distributed.Static (staticClosure)
--import Control.Distributed.Process
--import Control.Distributed.Process hiding (liftIO)
--import Control.Distributed.Process.Serializable
--import Control.Distributed.Process.Closure
--import Control.Distributed.Process.Node hiding (newLocalNode)
--import Control.Distributed.Process.Backend.SimpleLocalnet
--import Control.Concurrent hiding (newChan)
--import qualified Control.Concurrent as C
--import Control.Monad
--import Control.Monad.State hiding (liftIO)
--import Control.Applicative
--import Control.Monad.Trans hiding (liftIO)
--import Data.Binary hiding (get)
--import Data.Monoid
--import Data.Typeable
--import Data.IORef
--import qualified Data.Set as S
--import qualified Data.Map as M

--terminateSlave :: NodeId -> Process ()
--printProcess s = liftIO $ putStrLn s

printProcess :: String -> Process ()
printProcess s = liftIO $ putStrLn s

remotable ['printProcess]

ping = do
  --(sport, rport) <- newChan
  --liftIO . putStrLn $ show $ sendPortId sport
  spid <- getSelfPid
  liftIO . putStrLn $ show $ spid
  --return ()

master :: Backend -> [NodeId] -> Process ()
master backend slaves = do
  ping
  -- Do something interesting with the slaves
  liftIO . putStrLn $ "Found slaves: " ++ show slaves
  -- Terminate the slaves when the master terminates (this is optional)
  mapM_ terminateSlave slaves
  --master backend slaves
  --terminateAllSlaves backend

go :: String -> Backend -> [NodeId] -> Process ()
go "remotePrint" dc ns = do
  mapM_ tellPrint ns
  return ()
  where
    tellPrint n = do
      liftIO . putStrLn $ "Slaves: " ++ show n
      spawn n $ $(mkClosure 'printProcess) "hi there!"

go "rcv" dc ns = do
  () <- expect
  return ()
go "pong" dc ns = do
  liftIO $ putStrLn "Pong IO within a process"
  return ()
go "ping" dc ns = do
  liftIO $ putStrLn "Ping IO within a process"
  return ()

configSimpleLocalnetBackend :: String -> String -> IO Backend
configSimpleLocalnetBackend host port = initializeBackend host port initRemoteTable

main :: IO ()
main = do
  let host = "localhost"

  [serverType, port] <- getArgs

  case serverType of
    "master" -> do
      putStrLn $ "Starting master on port: " ++ port
      backend <- configSimpleLocalnetBackend host port
      startMaster backend $ master backend  
    "slave" -> do
      putStrLn $ "Starting slave on port: " ++ port
      backend <- configSimpleLocalnetBackend host port
      startSlave backend
    "bla" -> do
      -- Set up the context
      --hostName <- getHostName
      distributedContext <- initializeBackend "localhost" port initRemoteTable
      -- The first thing a context lets you do is create a node.
      node <- newLocalNode distributedContext
      peers <- findPeers distributedContext 2000
      liftIO . putStrLn $ "Slaves: " ++ show peers
      runProcess node (go serverType distributedContext peers)
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
