{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, 
TupleSections, ScopedTypeVariables, TemplateHaskell, DeriveGeneric #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

import Control.Concurrent (threadDelay)

import System.Environment (getArgs)
import Control.Distributed.Process

import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Closure

import Control.Distributed.Process.Node hiding (newLocalNode)
import Data.Binary
import GHC.Generics (Generic)

import Data.Typeable
import Data.DeriveTH

data PingPongMessage = Ping ProcessId
                     | Pong ProcessId
                     deriving (Generic, Typeable)

$( derive makeBinary ''PingPongMessage )

pingClient :: ProcessId -> Process ()
pingClient serverPid = do
    self <- getSelfPid

    send serverPid (Ping self)
    send serverPid (Ping self)
    send serverPid (Ping self)
    
    Pong from <- expect
    liftIO . putStrLn $ "OTHER CLIENT IMPLEMENTATION - got pong from: " ++ show from
    liftIO $ threadDelay (1*1000000)
    pingClient serverPid

remotable ['pingClient]

configSimpleLocalnetBackend :: String -> String -> IO Backend
configSimpleLocalnetBackend host port = initializeBackend host port $ __remoteTable initRemoteTable


main :: IO ()
main = do
    let host = "localhost"

    args <- getArgs

    case args of
        ["slave", port] -> do
            putStrLn $ "Starting slave on port: " ++ port
            backend <- configSimpleLocalnetBackend host port
            startSlave backend
        other -> do
            putStrLn $ "Unkown command: " ++ show other

