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
--import Control.Distributed.Static (staticClosure)
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

data Ping = Ping (SendPort Pong)
             | Blub
    deriving (Generic, Typeable)
data Pong = Pong ProcessId deriving (Generic, Typeable)

data Foo = Foo
    deriving (Generic)

newtype MonitorRefTypeable = MonitorRefTypeable MonitorRef deriving (Generic, Typeable, Show)

-- GHC will automatically fill out the instance
--instance Binary Foo
--instance Binary Ping
--instance Binary Pong
-- $( derive makeBinary ''Ping )

printProcess :: (ProcessId, String) -> Process ()
printProcess (_, s) = liftIO $ putStrLn s

--newtype ServerState = ServerState Int deriving (Binary, Typeable)

--data ServerState = Init
--                 | Waiting
--                 deriving( Typeable, Binary ) 

--instance Binary ServerState where
--       put (Init)          = do put (0 :: Word8)
--       put (Waiting)     = do put (1 :: Word8)
 
--       get = do t <- get :: Get Word8
--                case t of
--                     0 -> return Init
--                     1 -> return Waiting

--chatServer :: 



-- Bsp. Serializierung
--Processes can send data if the type implements the Serializable typeclass, which is done indirectly by implementing Binary and deriving Typeable

-- Beispiel Static Closure

-- Beispiel mit mkClosure, z. B. versenden von allgemeiner Funktion oder so; Erlang Book fib/fac => irgendeine OP
-- => Env. und auch mehrer Parameter als Tupel möglich

--sampleTask2 :: (ServerState, String) -> Process String
--sampleTask2 (t, s) = liftIO $ threadDelay (1*1000000) >> return s

sampleTask :: (Int, String) -> Process String
sampleTask (t, s) = liftIO $ threadDelay (t*1000000) >> return s

rcv = do
    () <- expect
    return ()

-- In order to spawn a process remotely we will need to configure the remote table (see the documentation for more details) and the easiest way to do this, is to let the library generate the relevant code for us. For example (taken from the distributed-process-platform test suites):
-- The call to remotable generates a remote table and generates a definition __remoteTable :: RemoteTable -> RemoteTable in our module for us. We can compose this with other remote tables in order to come up with a final, merged remote table for use in our program:
remotable ['printProcess, 'sampleTask, 'rcv]

bla = ($(mkClosure 'sampleTask) (2 :: Int, "foobar"))

-- Beispiel mit freien Variablen

-- Beispiel mit Typed Channels
--receive

-- Beispiel mit falschen typed Channels?

-- Beispiel mit expect :: Int bla -> falsch / untypisiert
--expect

-- Beispiel mit Azure Backend

-- Beispiel mit match / matchIf => zeigen das anhand der Typensignatur ausgewertet wird
--The match construct allows you to construct a list of potential message handlers and have them evaluated against incoming messages

-- Bsp. mit recieve timeout
--m <- expectTimeout 1000000
--    case m of
--      -- Die immediately - throws a ProcessExitException with the given reason.
--      Nothing  -> die "nothing came back!"
--      (Just s) -> say $ "got back " ++ s
--    return ()

--newClient :: Serializable a => (SendPort String, String) -> Process ()
newClient :: (SendPort String, String) -> Process ()
newClient (sendP, "hello") = do liftIO . putStrLn $ "Master started: "

--logMessage :: String -> Process ()
--logMessage msg = say $ "handling " ++ msg

type ChatServerSearchReply = Maybe ProcessId

--data ChatProtocol = JoinChat { clientName :: String,
--                                clientPid :: ProcessId }
--                  | ChatMessage { from :: ProcessId,
--                                   msg :: String, num :: Integer }
--                  | LeaveChat { clientPid :: ProcessId }
--                  deriving (Generic, Typeable, Show)

data JoinChatMessage = JoinChat { clientName :: String,
                                   clientPid :: ProcessId }
                     deriving (Generic, Typeable, Show)

data ChatMessage = ChatMessage {     from :: ProcessId,
                                  message :: String }
                 deriving (Generic, Typeable, Show)

--instance Binary ChatProtocol
-- derive uses TH to generate the instance automatically
$( derive makeBinary ''JoinChatMessage )
$( derive makeBinary ''ChatMessage )
$( derive makeBinary ''MonitorRefTypeable)

type ChatServerClient = (String, MonitorRefTypeable)
type ChatServerClients = [ChatServerClient]
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

--data ChatServerSearchReply = Found ProcessId
--                           | NotFound
--                           deriving (Generic, Typeable)

searchChatServer :: Backend -> String -> Process ChatServerSearchReply
searchChatServer backend chatRoom =
    searchChatServer' =<< liftIO (findPeers backend 2000)
    where
        searchChatServer' :: [NodeId] -> Process ChatServerSearchReply
        searchChatServer' (peer : tail) = do

            flip send "Message while searching for a chat server..." =<< getSelfPid

            whereisRemoteAsync peer chatRoom

            WhereIsReply name remoteWhereIs <- expect
            case remoteWhereIs of
                Just chatServerPid -> return (Just chatServerPid)
                otherwise          -> searchChatServer' tail

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
        reply $ Just chatServerPid = return chatServerPid


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
                            let message = ": " ++ msg
                            say $ show pid ++ message
                            broadcastMessage self pid message clients
                            serverLoop self clients (message : msgHistory)
                       ),
                 match (\(JoinChat {clientName=name, clientPid=chatClientPid}) -> do
                            say $ name ++ " is joining the chat..."
                            clientMonitorRef <- monitor chatClientPid
                            let newClients = Map.insert chatClientPid (mkChatServerClient name clientMonitorRef) clients
                            broadcastMessage self chatClientPid " is joining the chat..." newClients
                            sendMsgHistoryToNewClient self chatClientPid $ reverse msgHistory
                            serverLoop self newClients msgHistory
                        ),
                 match (\(ProcessMonitorNotification monitorRef processId _) -> do
                            say $ show processId ++ " is leaving the chat..."
                            broadcastMessage self processId " is leaving the chat..." clients
                            serverLoop self (Map.delete processId clients) msgHistory
                       ),
                 match (\(message :: String) -> do
                            say $ "Unkown message recieved: " ++ message
                            serverLoop self clients msgHistory
                       ),
                 matchUnknown $ serverLoop self clients msgHistory
                ]

        broadcastMessage :: ProcessId -> ProcessId -> String -> ChatServerClientMap -> Process ()
        broadcastMessage serverPid from message clients = do
            let client = clients Map.! from
            let groupMsg = chatClientName client ++ message
            mapM_ (\(clientPid, chatClient) -> send clientPid $ mkChatMessage serverPid groupMsg) . filter (\(clientPid, _) -> from /= clientPid) $ Map.toList clients

        sendMsgHistoryToNewClient :: ProcessId -> ProcessId -> ChatMessageHistory -> Process ()
        sendMsgHistoryToNewClient serverPid _ [] = return ()
        sendMsgHistoryToNewClient serverPid clientPid (msg : tail) = send clientPid (mkChatMessage serverPid msg) >> sendMsgHistoryToNewClient serverPid clientPid tail


-- Beispiele
-- TODO: Chat history, Reconnect der Clients beim server falls Server down...
-- Room chat server
-- MessageQueue => Other Message bei der registrierung! Interessant, da erst expect sich die gewünscht holt und die andere liegen lässt / => matchUnkown leert die Queue
-- Tail recursive
-- Link => Server <- Clients, damit alles Clients sterben sobald der Server weg ist / Note that link provides unidirectional linking
-- Monitor => Server -> Clients, damit der Server merkt das Clients weg sind
-- Say => Logging
-- Data ChatMessage vs. ChatProtocl => Probeleme bei mehreren Verzweigungen... Wahrscheinlich durch derive, Serializable selber implementieren
-- Sending String vs. => Data / send serverPid (msg :: String) <=> send serverPid $ mkChatMessage self msg),
-- Match vs. MatchIf
-- Search Server => findPeers / register on Node
-- Register master in Registry



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

            -- handle the user chat clint logic
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
                        matchIf (\(ChatMessage {from = pid, message = msg}) -> consolePid == pid)
                                (\(ChatMessage {from = pid, message = msg}) -> do
                                    --send serverPid (msg :: String)
                                    send serverPid $ mkChatMessage self msg),
                        matchIf (\(ChatMessage {from = pid, message = msg}) -> pid == serverPid)
                                (\(ChatMessage {from = pid, message = msg}) -> liftIO . putStrLn $ "\r" ++ msg) 
                        ]

        Nothing -> liftIO . putStrLn $ "No Chat Server found..."




chatClient :: String -> Backend -> LocalNode -> Process ()
chatClient name backend node = do

    --peers <- findPeers backend 2000
    --liftIO . putStrLn $ "Slaves: " ++ show peers

    peers <- liftIO $ findPeers backend 2000

    searchChatServer backend "MASTER"

    liftIO . putStrLn $ "Slaves: " ++ show peers

    
    forM_ peers $ \peer -> do
        liftIO . putStrLn $ "Searching for master on peer:" ++ show peer ++ "..."
        _ <- whereisRemoteAsync peer "master"
        WhereIsReply name remoteWhereIs <- expect
        case remoteWhereIs of
            Just pid -> liftIO . putStrLn $ "FOUND: " ++ show pid
            otherwise -> liftIO . putStrLn $ "NOTFOUND"

    pid <- getSelfPid
    

    mPid <- whereis "bla"
    case mPid of
        Just pid -> liftIO . putStrLn $ "FOUND: " ++ show pid
        otherwise -> do
            liftIO . putStrLn $ "NOT FOUND"
            register "bla" pid

    liftIO . putStrLn $ "Chat client PID: " ++ show pid

    liftIO . putStrLn $ "starting chat client: " ++ name
    (sendP :: SendPort String, recP) <- newChan
    
    let doIt clientPid = do
        liftIO . putStrLn $ "sending hello client: " ++ show clientPid
        send clientPid (sendP, "hello")

    -- Spawn worker inside one more process on the local node 
    _ <- spawnLocal $ forever $ do
        -- Test the matches in order against each message in the queue
        otherClients <- findSlaves backend
        liftIO . putStrLn $ "Clients: " ++ show otherClients
        liftIO . putStrLn $ "sending hello client: "
        receiveWait [match newClient]

    otherClients <- findSlaves backend
    liftIO . putStrLn $ "Clients: " ++ show otherClients
    mapM_ doIt otherClients
    --forever $ liftIO . putStrLn =<< receiveChan recP
    --forever . receiveWait $ [match logMessage]

    forever $ receiveWait
        [match newClient]
    --forever $ receiveWait 
    --    [
    --      match $ \(x :: String) -> liftIO . putStrLn $ "starting chat client: " ++ x
    --    ]    
    --return ()
        -- Test the matches in order against each message in the queue

ping = do
    --(sport, rport) <- newChan
    --liftIO . putStrLn $ show $ sendPortId sport
    spid <- getSelfPid
    liftIO . putStrLn $ show $ spid
    --return ()

master :: Backend -> [NodeId] -> Process ()
master backend slaves = do
    spid <- getSelfPid
    liftIO . putStrLn $ "Master started: " ++ show spid
    -- Do something interesting with the slaves
    liftIO . putStrLn $ "Found some slaves: " ++ show slaves
    -- Terminate the slaves when the master terminates (this is optional)
    liftIO $ threadDelay (1*1000000)
    terminateAllSlaves backend

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
configSimpleLocalnetBackend host port = initializeBackend host port $ __remoteTable initRemoteTable

main :: IO ()
main = do
    let host = "localhost"
    --let host = getHostName

    --[port, serverType | _] <- getArgs

    args <- getArgs

    case args of
        ["master", port] -> do
            putStrLn $ "Starting master on port: " ++ port
            backend <- configSimpleLocalnetBackend host port
            startMaster backend $ master backend  
        ["slave", port] -> do
            putStrLn $ "Starting slave on port: " ++ port
            backend <- configSimpleLocalnetBackend host port
            startSlave backend
        
        ["chat_server", port, chatRoom] -> do
            distributedContext <- initializeBackend host port remoteTable
            node <- newLocalNode distributedContext

            runProcess node (startChatServer distributedContext chatRoom)

        
        ["chat_client", name, port, chatRoom] -> do
            distributedContext <- initializeBackend host port remoteTable
            node <- newLocalNode distributedContext

            runProcess node (startChatClient distributedContext chatRoom name)
            --peers <- findPeers distributedContext 2000
            --liftIO . putStrLn $ "Slaves: " ++ show peers
            ----otherClients <- findSlaves distributedContext
            ----liftIO . putStrLn $ "Clients: " ++ show otherClients

            --runProcess node (chatClient name distributedContext node)

        [serverType, port] -> do
            -- Set up the context
            --hostName <- getHostName
            distributedContext <- initializeBackend host port remoteTable
            -- The first thing a context lets you do is create a node.
            node <- newLocalNode distributedContext
            peers <- findPeers distributedContext 2000
            liftIO . putStrLn $ "Slaves: " ++ show peers
            runProcess node (go serverType distributedContext peers)
        other -> do
            putStrLn $ "Unkown command: " ++ show other
    where
        remoteTable = (__remoteTable initRemoteTable)


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
