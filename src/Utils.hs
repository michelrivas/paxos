-----------------------------------------------------------------------------
--
-- Module      :  Utils
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :  m.rivas1@uni.brighton.ac.uk
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Utils (
    ServerID,
    Server(..),
    Proposal(..),
    ServerState(..),
    MessageType,
    Message(..),
    broadcast,
    send,
    parseHostPort,
    parseMessage,
    checkConnection,
    saveServer
) where

import Network (PortID(..), Socket,PortNumber)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle)
import Control.Concurrent.MVar
import Data.List.Split

--| type ServerID
type ServerID = String

--| data Server
data Server = Server {
    serverID :: ServerID,
    serverHandle :: Handle,
    hostName :: String,
    portNumber :: PortNumber
}

--| data Proposal
data Proposal = Proposal {
    proposalID :: ServerID,
    proposalValue :: Int
}

--| data ServerState
data ServerState = ServerState {
    localID :: ServerID,
    proposalNumber :: Int,
    highestProposal :: Proposal,
    localHost :: String,
    localPort :: PortNumber,
    prepareQuorum :: Int,
    acceptQuorum :: Int,
    serverList :: [Server],
    values :: [Int]
}

--| type MessageType
type MessageType = String

--| data Message
data Message = Message{
    messageType :: MessageType,
    messageId :: ServerID,
    messageValue :: Int
}

--| parseHostPort
parseHostPort :: String -> (String, String)
parseHostPort hostPort = (\(x:y)->(x,head y)) $ splitOn ":" hostPort

--| parseMessage
parseMessage :: ServerID -> String -> Message
parseMessage id text = do 
    let (mType : mValue : _) = splitOn ":" text
    Message {messageType = mType, messageId = id, messageValue = fromIntegral (read mValue :: Int)}

--| checkConnection
checkConnection :: PortNumber -> [Server] -> Bool
checkConnection port servers = do
    case servers of
        [] -> False
        _  -> and $ map (\x -> portNumber x == port) servers

--| saveServer
saveServer :: ServerState -> Server -> ServerState
saveServer state server = do
    let port = portNumber server
    let servers = serverList state
    let isConnected = (length servers > 0) && (and $ map (\x -> portNumber x == port) servers)
    case not isConnected of 
        True -> state { serverList = server : servers}
        False -> state

--| send ::
send :: String -> Handle -> IO ()
send msg handle = hPutStrLn handle msg

--| broadcast
broadcast :: ServerState -> String -> IO ()
broadcast state msg = broadcastServers msg $ serverList state

--| broadcastServers
broadcastServers :: String -> [Server] -> IO ()
broadcastServers msg servers = mapM_ (send msg . serverHandle) servers

