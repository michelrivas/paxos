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
    checkConnection
) where

import Network (PortID(..), Socket,PortNumber)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle)
import Control.Concurrent.MVar
import Data.List.Split

type ServerID = String

data Server = Server {
    serverID :: ServerID,
    serverHandle :: Handle,
    hostName :: String,
    portNumber :: PortNumber
}

data Proposal = Proposal {
    proposalID :: ServerID,
    proposalValue :: Int
}

data ServerState = ServerState {
    localID :: ServerID,
    proposalNumber :: Int,
    highestProposal :: Proposal,
    localHost :: String,
    localPort :: PortNumber,
    prepareQuorum :: Int,
    acceptQuorum :: Int,
    serverList :: [Server]
}

type MessageType = String

data Message = Message{
    messageType :: MessageType,
    messageId :: ServerID,
    messageValue :: Int
}

parseHostPort :: String -> (String, String)
parseHostPort hostPort = (\(x:y)->(x,head y)) $ splitOn ":" hostPort

parseMessage :: ServerID -> String -> Message
parseMessage id text = do 
    let (mType : mValue : _) = splitOn ":" text
    Message {messageType = mType, messageId = id, messageValue = fromIntegral (read mValue :: Int)}

checkConnection :: PortNumber -> [Server] -> Bool
checkConnection port servers = do
    case servers of
        [] -> False
        _  -> and $ map (\x -> portNumber x == port) servers

send :: String -> Handle -> IO ()
send msg handle = hPutStrLn handle msg
--    putStrLn $ "Sent: " ++ msg

broadcast :: MVar ServerState -> String -> IO ()
broadcast config msg = do
    state <- readMVar config
    --putMVar config state
    broadcastServers msg $ serverList state

broadcastServers :: String -> [Server] -> IO ()
broadcastServers msg servers = mapM_ (send msg . serverHandle) servers

