-------------------------------Acceptor.hs----------------------------------------------
--
-- Module      :  Proposer
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

module Proposer (
    prepareRequest,
    prepareAccepted,
    acceptRequest,
    acceptAccepted,
    decideValue
) where

import Control.Concurrent.MVar
--import Control.Concurrent (threadDelay)

import Utils

-- PROPOSER
prepareRequest :: String -> String
prepareRequest value = "1:" ++ value

prepareAccepted :: MVar ServerState -> Server -> Message -> IO ()
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

acceptRequest :: MVar ServerState -> IO ()
acceptRequest config = do
    --threadDelay 5000000
    state <- readMVar config
    let value = proposalNumber state
    --putMVar config state
    broadcast config ("3:" ++ show value)

acceptAccepted :: MVar ServerState -> Server -> Message -> IO ()
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

decideValue :: MVar ServerState -> IO ()
decideValue config = do
    --threadDelay 5000000
    state <- readMVar config
    let value = proposalNumber state
    --putMVar config state
    broadcast config ("5:" ++ show value)




