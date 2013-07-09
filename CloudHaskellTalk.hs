{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, 
TupleSections, ScopedTypeVariables, TemplateHaskell, DeriveGeneric #-}

import System.IO
--import Control.Concurrent (threadDelay)

import System.Environment (getArgs)
import Control.Distributed.Process

--import Control.Distributed.Process.Node (initRemoteTable, runProcess)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Closure

--import Control.Distributed.Process.Serializable

import Control.Distributed.Process.Node hiding (newLocalNode)

import Control.Monad

import Data.Binary
import GHC.Generics (Generic)


import Data.Typeable
import Data.DeriveTH

--import Data.Set (Set)
--import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map


newtype MonitorRefTypeable = MonitorRefTypeable MonitorRef deriving (Generic, Typeable, Show)

remotable []

type ChatServerSearchReply = Maybe ProcessId

data JoinChatMessage = JoinChat { clientName :: String,
                                   clientPid :: ProcessId }
                     deriving (Generic, Typeable, Show)

data ChatMessage = ChatMessage {     from :: ProcessId,
                                  message :: String }
                 deriving (Generic, Typeable, Show)

$( derive makeBinary ''JoinChatMessage )
$( derive makeBinary ''ChatMessage )
$( derive makeBinary ''MonitorRefTypeable)

type ChatServerClient = (String, MonitorRefTypeable)

type ChatMessageHistory = [String]

type ChatServerClientMap = Map ProcessId ChatServerClient

mkChatServerClient :: String -> MonitorRef -> ChatServerClient
mkChatServerClient name monitorRef = (name, MonitorRefTypeable monitorRef)

mkJoinChatMessage :: ProcessId -> String -> JoinChatMessage
mkJoinChatMessage pid msg = JoinChat {clientPid = pid, clientName = msg}

mkChatMessage :: ProcessId -> String -> ChatMessage
mkChatMessage pid msg = ChatMessage {from = pid, message = msg}

chatClientName :: ChatServerClient -> String
chatClientName (name, _) = name

searchChatServer :: Backend -> String -> Process ChatServerSearchReply
searchChatServer backend chatRoom =
    searchChatServer' =<< liftIO (findPeers backend 2000)
    where
        searchChatServer' :: [NodeId] -> Process ChatServerSearchReply
        searchChatServer' (peer : xs) = do

            flip send "Message while searching for a chat server..." =<< getSelfPid

            whereisRemoteAsync peer chatRoom

            WhereIsReply _name remoteWhereIs <- expect
            case remoteWhereIs of
                Just chatServerPid -> return (Just chatServerPid)
                _                  -> searchChatServer' xs

        searchChatServer' [] = return Nothing

initChatServer :: Backend -> String -> Process ProcessId
initChatServer backend chatRoom =
    reply =<< searchChatServer backend chatRoom
    where
        reply :: ChatServerSearchReply -> Process ProcessId
        reply Nothing = do
            chatServerPid <- getSelfPid
            register chatRoom chatServerPid
            return chatServerPid
        reply (Just chatServerPid) = return chatServerPid


startChatServer :: Backend -> String -> Process ()
startChatServer backend chatRoom = do
    foundChatServerPid <- initChatServer backend chatRoom
    serve foundChatServerPid =<< getSelfPid
    where
        serve :: ProcessId -> ProcessId -> Process ()
        serve pid self
            | pid == self = say "Starting Chat Server..." >> serverLoop self Map.empty []
            | otherwise = liftIO . putStrLn $ "Chat Server already started with the PID: " ++ show pid ++ "..."

        serverLoop :: ProcessId -> ChatServerClientMap -> ChatMessageHistory -> Process()
        serverLoop self clients msgHistory =
            receiveWait
                [
                 match (\(ChatMessage {from=pid, message=msg}) -> do
                            let sendMessage = ": " ++ msg
                            say $ show pid ++ sendMessage
                            broadcastMessage self pid sendMessage clients
                            serverLoop self clients (sendMessage : msgHistory)
                       ),
                 match (\(JoinChat {clientName=name, clientPid=chatClientPid}) -> do
                            say $ name ++ " is joining the chat..."
                            clientMonitorRef <- monitor chatClientPid
                            let newClients = Map.insert chatClientPid (mkChatServerClient name clientMonitorRef) clients
                            broadcastMessage self chatClientPid " is joining the chat..." newClients
                            sendMsgHistoryToNewClient self chatClientPid $ reverse msgHistory
                            serverLoop self newClients msgHistory
                        ),
                 match (\(ProcessMonitorNotification _monitorRef processId _) -> do
                            say $ show processId ++ " is leaving the chat..."
                            broadcastMessage self processId " is leaving the chat..." clients
                            serverLoop self (Map.delete processId clients) msgHistory
                       ),
                 match (\(msg :: String) -> do
                            say $ "Unkown message recieved: " ++ msg
                            serverLoop self clients msgHistory
                       ),
                 matchUnknown $ serverLoop self clients msgHistory
                ]

        broadcastMessage :: ProcessId -> ProcessId -> String -> ChatServerClientMap -> Process ()
        broadcastMessage serverPid fromPid msg clients = do
            let client = clients Map.! fromPid
            let groupMsg = chatClientName client ++ msg
            mapM_ (\(cPid, _chatClient) -> send cPid $ mkChatMessage serverPid groupMsg) . filter (\(cPid, _) -> fromPid /= cPid) $ Map.toList clients

        sendMsgHistoryToNewClient :: ProcessId -> ProcessId -> ChatMessageHistory -> Process ()
        sendMsgHistoryToNewClient _serverPid _ [] = return ()
        sendMsgHistoryToNewClient serverPid cPid (msg : xs) = send cPid (mkChatMessage serverPid msg) >> sendMsgHistoryToNewClient serverPid cPid xs



startChatClient :: Backend -> String -> String -> Process ()
startChatClient backend chatRoom name = do
    chatClientPid <- getSelfPid
    csReply <- searchChatServer backend chatRoom

    case csReply of
        Just chatServerPid -> do
            link chatServerPid

            liftIO . putStrLn $ "Chat Server found!..."

            -- joining the chat
            send chatServerPid $ mkJoinChatMessage chatClientPid name

            -- spwan the terminal input process to watch for user input
            consoleInputPid <- spawnLocal $ forever $ consoleInputProcess
            link consoleInputPid

            -- handle the user chat client logic
            forever $ handleChatClient consoleInputPid chatServerPid

            where
                consoleInputProcess :: Process ()
                consoleInputProcess = do
                    link chatClientPid
                    pid <- getSelfPid
                    liftIO . putStr $ "Message: "
                    liftIO $ hFlush stdout
                    msg <- liftIO getLine
                    if not $ null msg
                        then send chatClientPid $ mkChatMessage pid msg
                        else return ()

                handleChatClient :: ProcessId -> ProcessId -> Process ()
                handleChatClient consolePid serverPid = do
                    self <- getSelfPid
                    receiveWait [
                        matchIf (\(ChatMessage {from = pid, message = _msg}) -> consolePid == pid)
                                (\(ChatMessage {from = _pid, message = msg}) -> do
                                    send serverPid $ mkChatMessage self msg),
                        matchIf (\(ChatMessage {from = pid, message = _msg}) -> pid == serverPid)
                                (\(ChatMessage {from = _pid, message = msg}) -> liftIO . putStrLn $ "\r" ++ msg) 
                        ]

        Nothing -> liftIO . putStrLn $ "No Chat Server found..."

configSimpleLocalnetBackend :: String -> String -> IO Backend
configSimpleLocalnetBackend host port = initializeBackend host port $ __remoteTable initRemoteTable

main :: IO ()
main = do
    let host = "localhost"

    args <- getArgs

    case args of
        
        ["chat_server", port, chatRoom] -> do
            distributedContext <- configSimpleLocalnetBackend host port
            node <- newLocalNode distributedContext

            runProcess node (startChatServer distributedContext chatRoom)

        
        ["chat_client", name, port, chatRoom] -> do
            distributedContext <- configSimpleLocalnetBackend host port
            node <- newLocalNode distributedContext

            runProcess node (startChatClient distributedContext chatRoom name)

        other -> do
            putStrLn $ "Unkown command: " ++ show other

