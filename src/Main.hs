-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

-- This strang looking comment adds code only needed when running the
-- doctest tests embedded in the comments
-- $setup
-- >>> import Data.List (stripPrefix)

-- | Simple function to create a hello message.
-- prop> stripPrefix "Hello " (hello s) == Just s
import Network (listenOn, withSocketsDo, accept, connectTo, sClose, PortID(..), Socket,PortNumber)
import System.Environment (getArgs)
import System.IO (hSetBuffering, hGetLine, hClose, BufferMode(..), Handle)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Monad
import Control.Exception
import Data.GUID
import Data.List.Split(splitOn)
import Data.Maybe(fromJust)

import Utils
import Proposer
import Acceptor


-- | Generates a GUID for node IDs
newGUID :: IO String
newGUID = genString


-- | Main server thread
main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    args <- map parseHostPort <$> getArgs
    id <- newGUID
    let (host, p) = head args
    let port = fromIntegral (read $ p :: Int)
    socket <- listenOn $ PortNumber port
    putStrLn $ "Listening on " ++ host ++ ":" ++ p
    putStrLn $ "ServerID: " ++ id
    let state = ServerState {
        localID = id, 
        proposalNumber = 1, 
        localHost = host, 
        localPort = port, 
        serverList = [], 
        prepareQuorum = 0, 
        acceptQuorum = 0, 
        learnedValues = [], 
        highestProposal = Proposal{
            proposalID = id, 
            proposalValue = 0
        }
    }
    config <- newMVar state
    forkIO $ connectServers config (tail args)
    forkIO $ mainProcess config
    forever $ accept socket >>= forkIO . (handleClientConnection config)


-- | Set up listening channel for one client
handleClientConnection :: MVar ServerState -> (Handle, String, PortNumber) -> IO ()
handleClientConnection config (handle, host, portno) = do
    putStrLn "Client connected"
    id <- hGetLine handle
    putStrLn $ "ID: " ++ id
    state <- readMVar config
    send (localID state) handle
    let server = Server {
        serverID = id, 
        serverHandle = handle, 
        hostName = host, 
        portNumber = portno
    }
    serverState <- takeMVar config
    putMVar config $ saveServer serverState server
    handleClientRequest config server


-- | Listening channel thread between one client and the server
handleClientRequest :: MVar ServerState -> Server -> IO ()
handleClientRequest config server = do
    text <- hGetLine $ serverHandle server
    let id = serverID server
    let msg = parseMessage id text
    case messageType msg of
        "1" -> (do 
                    state <- takeMVar config
                    let (proposalState, proposalMsg) = checkProposal state msg
                    putMVar config proposalState
                    when (proposalMsg /= Nothing) $ (send (fromJust proposalMsg) $ serverHandle server)
                    putStrLn $ "Accepted prepare: " ++ show (messageValue msg)
                )
        "2" -> (do
                    state <- takeMVar config
                    let (prepareState, prepareMsg) = prepareAccepted state msg
                    putMVar config prepareState
                    when (prepareMsg /= Nothing) $ (broadcast prepareState $ fromJust prepareMsg)
                    putStrLn $ "Prepare accepted: " ++ show (messageValue msg)
                )
        "3" -> (do
                    state <- takeMVar config
                    let (acceptState, acceptMsg) = checkAccept state msg
                    putMVar config acceptState
                    when (acceptMsg /= Nothing) $ (send (fromJust acceptMsg) $ serverHandle server)
                    putStrLn $ "Accepted accept: " ++ show (messageValue msg)
                )
        "4" -> (do
                    state <- takeMVar config
                    let (acceptedState, acceptedMsg) = acceptAccepted state msg
                    case acceptedMsg of
                        Just m ->  (do
                                        putMVar config acceptedState {
                                            learnedValues = (messageValue msg) : (learnedValues acceptedState)
                                        }
                                        broadcast acceptedState m
                                        putStrLn $ "P: " ++ show (learnedValues acceptedState)
                                    )
                        Nothing -> (do
                                        putMVar config acceptedState
                                        putStrLn $ "Accepted accepted: " ++ show (messageValue msg)
                                    )
                )
        "5" -> (do
                    state <- takeMVar config
                    let decidedState = valueDecided state msg
                    putMVar config decidedState
                    putStrLn $ "A: " ++ show (learnedValues decidedState)
                )
        _   -> putStrLn text
    handleClientRequest config server


-- | Set up connections to other servers from params
connectServers :: MVar ServerState -> [(String, String)] -> IO ()
connectServers _ [] = return ()
connectServers config ((host, portno) : hosts) = do
    forkIO $ connectServer config host portno
    connectServers config hosts


-- | Set up connection with a single client
connectServer :: MVar ServerState -> String -> String -> IO ()
connectServer config host portno = do
    let port = fromIntegral (read portno :: Int)
    putStrLn $ "Connecting to " ++ host ++ ":" ++ portno
    result <- testAddress host (PortNumber port)
    case result of
        Just handle -> do
            state <- readMVar config
            id <- handShake state handle
            let server = Server {
                serverID = id, 
                serverHandle = handle, 
                hostName = host, 
                portNumber = port
            }
            serverState <- takeMVar config
            putMVar config $ saveServer serverState server
            putStrLn $ "Servers: " ++ show (1 + (length $ serverList state))
            putStrLn $ "Connected to " ++ host ++ ":" ++ portno
            handleClientRequest config server
        Nothing -> do
            putStrLn "Error connecting"
            threadDelay 5000000
            connectServer config host portno


-- | Tests if an IP:Port pair is valid
testAddress :: String -> PortID -> IO (Maybe Handle)
testAddress host port = do
    result <- try $ connectTo host port
    case result of
        Left (SomeException e) -> return Nothing
        Right h -> return $ Just h


-- | Exchange IDs with another server
handShake :: ServerState -> Handle -> IO String
handShake state handle = do
    send (localID state) handle
    hGetLine handle


-- | Gets input from user and starts a Paxos instance with a prepare request
-- | User input can be numbers
mainProcess :: MVar ServerState -> IO ()
mainProcess config = do
    line <- getLine
    state <- takeMVar config
    let newState = state {proposalNumber = fromIntegral (read line :: Int)}
    putMVar config newState
    broadcast newState $ prepareRequest line
    mainProcess config


