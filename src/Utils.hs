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
    parseHostPort
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
parseHostPort hostPort = (\(x:y:_)->(x,y)) $ splitOn ":" hostPort

send :: String -> Handle -> IO ()
send msg handle = hPutStrLn handle msg
--    putStrLn $ "Sent: " ++ msg

broadcast :: MVar ServerState -> String -> IO ()
broadcast config msg = do
    state <- takeMVar config
    let servers = serverList state
    putMVar config state
    broadcastServers msg servers

broadcastServers :: String -> [Server] -> IO ()
broadcastServers msg servers = mapM_ (send msg . serverHandle) servers

