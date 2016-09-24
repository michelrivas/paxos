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
-- | Utility functions and data types
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
    majority,
    saveServer
) where

import Network (PortID(..), Socket,PortNumber)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle)
import Control.Concurrent.MVar
import Data.List.Split


-- | ServerID alias to String
type ServerID = String


-- | Server data type
data Server = Server {
    serverID :: ServerID,
    serverHandle :: Handle,
    hostName :: String,
    portNumber :: PortNumber
}


-- | Proposal data type
data Proposal = Proposal {
    proposalID :: ServerID,
    proposalValue :: Int
}


-- | Server state data type
data ServerState = ServerState {
    localID :: ServerID,
    proposalNumber :: Int,
    highestProposal :: Proposal,
    localHost :: String,
    localPort :: PortNumber,
    prepareQuorum :: Int,
    acceptQuorum :: Int,
    serverList :: [Server],
    learnedValues :: [Int]
}


-- | MessageType alias to string
type MessageType = String


-- | Message data type
data Message = Message{
    messageType :: MessageType,
    messageId :: ServerID,
    messageValue :: Int
}


-- | Parses a string in the form "host:port" into a tuple (host, port)
parseHostPort :: String -> (String, String)
parseHostPort hostPort = (\(host : port) ->
    (host, head port)) $ splitOn ":" hostPort


-- | Parses a String into a Message
parseMessage :: ServerID -> String -> Message
parseMessage id text = (\(mType : mValue) -> 
    Message {
        messageType = mType, 
        messageId = id, 
        messageValue = fromIntegral (read $ head mValue :: Int)
    }) $ splitOn ":" text


-- | Calculates the majority of the server set
majority :: ServerState -> Int
majority state = length (serverList state) `quot` 2 + 1


-- | Saves a server into the list of servers
saveServer :: ServerState -> Server -> ServerState
saveServer state server = do
    let port = portNumber server
    let servers = serverList state
    let isConnected = (length servers > 0) && (and $ map (\x -> portNumber x == port) servers)
    case not isConnected of 
        True -> state { serverList = server : servers}
        False -> state


-- | Sends a message to a client
send :: String -> Handle -> IO ()
send msg handle = hPutStrLn handle msg


-- | Sends a message to all the servers
broadcast :: ServerState -> String -> IO ()
broadcast state msg = broadcastServers msg $ serverList state


-- | Sends a message to all the servers
broadcastServers :: String -> [Server] -> IO ()
broadcastServers msg servers = mapM_ (send msg . serverHandle) servers

