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
    send
) where

import Network (PortID(..), Socket,PortNumber)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle)
import Control.Concurrent.MVar

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

send msg handle = do
    hPutStrLn handle msg
--    putStrLn $ "Sent: " ++ msg

broadcast config msg = do
    state <- takeMVar config
    let servers = serverList state
    putMVar config state
    broadcastServers config msg servers

broadcastServers _ _ [] = return ()
broadcastServers config msg (server : servers) = do
    send msg (serverHandle server)
    broadcastServers config msg servers





