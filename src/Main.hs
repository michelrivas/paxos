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
import System.IO (hSetBuffering, hGetLine, hPutStrLn, hClose, BufferMode(..), Handle)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Monad
import Control.Exception
import Data.GUID
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

newGUID :: IO String
newGUID = genString

main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    id <- newGUID
    let port = fromIntegral (read $ head args :: Int)
    socket <- listenOn $ PortNumber port
    putStrLn $ "Listening on " ++ (head args)
    putStrLn $ "ServerID: " ++ id
    let state = ServerState {localID = id, proposalNumber = 1, localPort = port, serverList = [], prepareQuorum = 0, acceptQuorum = 0, highestProposal = Proposal{proposalID = id, proposalValue = 0}}
    config <- newMVar state
    forkIO $ connectServers config (tail args)
    forkIO $ mainProcess config
    forever $ accept socket >>= forkIO . (handleClientConnection config)

handleClientConnection config (handle, host, portno) = do
    putStrLn "Client connected"
    id <- hGetLine handle
    putStrLn $ "ID: " ++ id
    sendID config handle
    let server = Server {serverID = id, serverHandle = handle, hostName = host, portNumber = portno}
    saveServer config server
    handleClientRequest config server

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

parseMessage id text = do 
    let (mType : mValue : _) = splitOn ":" text
    Message {messageType = mType, messageId = id, messageValue = fromIntegral (read mValue :: Int)}

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

connectServers _ [] = return ()
connectServers config (portno : ports) = do
    forkIO $ connectServer config "localhost" portno
    connectServers config ports

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

checkConnection config port = do
    state <- takeMVar config
    let servers = serverList state
    putMVar config state
    case servers of
        [] -> return False
        _  -> return $ and $ map (\x -> portNumber x == port) servers

checkServer portno server = do
    portNumber server == portno

testAddress host port = do
    result <- try $ connectTo host port
    case result of
        Left (SomeException e) -> return Nothing
        Right h -> return $ Just h

sendID config handle = do
    state <- takeMVar config
    send (localID state) handle
    putMVar config state

handShake config handle = do
    sendID config handle
    hGetLine handle

mainProcess config = do
    line <- getLine
    state <- takeMVar config
    let newState = state {proposalNumber = fromIntegral (read line :: Int)}
    putMVar config newState
    prepareRequest config line
    mainProcess config

-- PROPOSER
prepareRequest config value = do
    broadcast config ("1:" ++ value)

prepareAccepted config server msg = do
    state <- takeMVar config
    let quorum = (prepareQuorum state) + 1
--  Compare new value with proposed value
    let newState = state {prepareQuorum = quorum, highestProposal = Proposal {proposalID = messageId msg, proposalValue = messageValue msg}}
    putStrLn $ "Prepare accepted: " ++ show (messageValue msg)
    putMVar config newState
    let majority = length (serverList state) `quot` 2 + 1
    case compare quorum majority of
        LT -> return ()
        _  -> acceptRequest config

acceptRequest config = do
    threadDelay 5000000
    state <- takeMVar config
    let value = proposalNumber state
    putMVar config state
    broadcast config ("3:" ++ show value)

acceptAccepted config server msg = do
    state <- takeMVar config
    let quorum = (acceptQuorum state) + 1
    let newState = state {acceptQuorum = quorum}
    putStrLn $ "Accept accepted: " ++ show (messageValue msg)
    putMVar config newState
    let majority = length (serverList state) `quot` 2 + 1
    case compare quorum majority of
        LT -> return ()
        _  -> decideValue config

decideValue config = do
    threadDelay 5000000
    state <- takeMVar config
    let value = proposalNumber state
    putMVar config state
    broadcast config ("5:" ++ show value)

-- ACCEPTOR
checkProposal config server msg = do
    state <- takeMVar config
    let prop = highestProposal state
    putMVar config state
    case compare (messageValue msg) (proposalValue prop) of 
        LT -> return ()
        _  -> acceptPrepare config server msg

acceptPrepare config server msg = do
    threadDelay 5000000
    state <- takeMVar config
    let newState = state {proposalNumber = messageValue msg, highestProposal = Proposal {proposalID = messageId msg, proposalValue = messageValue msg}}
    putStrLn $ "Accepted prepare: " ++ show (messageValue msg)
    putMVar config newState
    send ("2:" ++ show (proposalNumber state)) (serverHandle server)

checkAccept config server msg = do
    state <- takeMVar config
    let prop = proposalNumber state
    putMVar config state
    case compare (messageValue msg) (prop) of 
        EQ -> acceptAccept config server msg
        _  -> return ()

acceptAccept config server msg = do
    threadDelay 5000000
    state <- takeMVar config
    let newState = state {proposalNumber = messageValue msg, highestProposal = Proposal {proposalID = messageId msg, proposalValue = messageValue msg}}
    putStrLn $ "Accepted accept: " ++ show (messageValue msg)
    putMVar config newState
    send ("4:" ++ show (proposalNumber state)) (serverHandle server)

valueDecided config server msg = do
    threadDelay 5000000
    state <- takeMVar config
    let newState = state {proposalNumber = 0, highestProposal = Proposal {proposalID = localID state, proposalValue = 0}}
    putStrLn $ "Final value: " ++ show (proposalNumber state)
    putMVar config newState
