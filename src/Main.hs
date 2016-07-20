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
import Data.List.Split

import Utils
import Proposer
import Acceptor


newGUID :: IO String
newGUID = genString

main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    id <- newGUID
    let (host, p) = parseHostPort $ head args
    let port = fromIntegral (read $ p :: Int)
    socket <- listenOn $ PortNumber port
    putStrLn $ "Listening on " ++ host ++ ":" ++ p
    putStrLn $ "ServerID: " ++ id
    let state = ServerState {localID = id, proposalNumber = 1, localHost = host, localPort = port, serverList = [], prepareQuorum = 0, acceptQuorum = 0, highestProposal = Proposal{proposalID = id, proposalValue = 0}}
    config <- newMVar state
    forkIO $ connectServers config (tail args)
    forkIO $ mainProcess config
    forever $ accept socket >>= forkIO . (handleClientConnection config)

handleClientConnection :: MVar ServerState -> (Handle, String, PortNumber) -> IO ()
handleClientConnection config (handle, host, portno) = do
    putStrLn "Client connected"
    id <- hGetLine handle
    putStrLn $ "ID: " ++ id
    sendID config handle
    let server = Server {serverID = id, serverHandle = handle, hostName = host, portNumber = portno}
    saveServer config server
    handleClientRequest config server

handleClientRequest :: MVar ServerState -> Server -> IO ()
handleClientRequest config server = do
    text <- hGetLine $ serverHandle server
    let id = serverID server
    let msg = parseMessage id text
    case messageType msg of
        "1" -> checkProposal config server msg
        "2" -> prepareAccepted config server msg
        "3" -> checkAccept config server msg
        "4" -> acceptAccepted config server msg
        "5" -> valueDecided config server msg
        _   -> putStrLn $ id ++ " says: " ++ text
    handleClientRequest config server

parseMessage :: String -> String -> Message
parseMessage id text = do 
    let (mType : mValue : _) = splitOn ":" text
    Message {messageType = mType, messageId = id, messageValue = fromIntegral (read mValue :: Int)}

saveServer :: MVar ServerState -> Server -> IO ()
saveServer config server = do
    let port = portNumber server
    state <- takeMVar config
    let servers = serverList state
    let isConnected = (length servers > 0) && (and $ map (\x -> portNumber x == port) servers)
    putStrLn $ "isConnected: " ++ show isConnected
    case not isConnected of 
        True -> (do
            let newState = state { serverList = server : servers}
            putMVar config newState
            putStrLn $ "Servers: " ++ show (1 + length servers))
        False -> putMVar config state


connectServers :: MVar ServerState -> [String] -> IO ()
connectServers _ [] = return ()
connectServers config (hostPort : hosts) = do
    let (host, portno) = parseHostPort hostPort
    forkIO $ connectServer config host portno
    connectServers config hosts

connectServer :: MVar ServerState -> String -> String -> IO ()
connectServer config host portno = do
    let port = fromIntegral (read portno :: Int)
    putStrLn $ "Connecting to " ++ host ++ ":" ++ portno
    result <- testAddress host (PortNumber port)
    case result of
        Just handle -> do
            id <- handShake config handle
            let server = Server {serverID = id, serverHandle = handle, hostName = host, portNumber = port}
            saveServer config server
            putStrLn $ "Connected to " ++ host ++ ":" ++ portno
            handleClientRequest config server
        Nothing -> do
            putStrLn "Error connecting"
            threadDelay 5000000
            connectServer config host portno

checkConnection :: MVar ServerState -> PortNumber -> IO Bool
checkConnection config port = do
    state <- readMVar config
    let servers = serverList state
    --putMVar config state
    case servers of
        [] -> return False
        _  -> return $ and $ map (\x -> portNumber x == port) servers

checkServer :: PortNumber -> Server -> Bool
checkServer portno server = do
    portNumber server == portno

testAddress :: String -> PortID -> IO (Maybe Handle)
testAddress host port = do
    result <- try $ connectTo host port
    case result of
        Left (SomeException e) -> return Nothing
        Right h -> return $ Just h

sendID :: MVar ServerState -> Handle -> IO ()
sendID config handle = do
    state <- readMVar config
    send (localID state) handle
    --putMVar config state

handShake :: MVar ServerState -> Handle -> IO String
handShake config handle = do
    sendID config handle
    hGetLine handle

mainProcess :: MVar ServerState -> IO ()
mainProcess config = do
    line <- getLine
    state <- takeMVar config
    let newState = state {proposalNumber = fromIntegral (read line :: Int)}
    putMVar config newState
    prepareRequest config line
    mainProcess config


