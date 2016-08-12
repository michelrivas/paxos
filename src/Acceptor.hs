-----------------------------------------------------------------------------
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

module Acceptor (
    checkProposal,
    checkAccept,
    acceptAccept,
    valueDecided
) where

import Control.Concurrent.MVar
--import Control.Concurrent (threadDelay)

import Utils

-- ACCEPTOR
checkProposal :: ServerState -> Message -> (ServerState, Maybe String)
checkProposal state msg = do
    let prop = highestProposal state
    --putMVar config state
    let newState = state {proposalNumber = messageValue msg, highestProposal = Proposal {proposalID = messageId msg, proposalValue = messageValue msg}}
    case compare (messageValue msg) (proposalValue prop) of 
        LT -> (state, Nothing)
        _  -> (newState, Just $ ("2:" ++) . show $ proposalNumber state)

checkAccept :: MVar ServerState -> Server -> Message -> IO ()
checkAccept config server msg = do
    state <- readMVar config
    let prop = proposalNumber state
    --putMVar config state
    case compare (messageValue msg) (prop) of 
        EQ -> acceptAccept config server msg
        _  -> return ()

acceptAccept :: MVar ServerState -> Server -> Message -> IO ()
acceptAccept config server msg = do
--    threadDelay 5000000
    state <- takeMVar config
    let newState = state {proposalNumber = messageValue msg, highestProposal = Proposal {proposalID = messageId msg, proposalValue = messageValue msg}}
    putStrLn $ "Accepted accept: " ++ show (messageValue msg)
    putMVar config newState
    send ("4:" ++ show (proposalNumber state)) (serverHandle server)

valueDecided :: MVar ServerState -> Server -> Message -> IO ()
valueDecided config server msg = do
--    threadDelay 5000000
    state <- takeMVar config
    let newState = state {proposalNumber = 0, highestProposal = Proposal {proposalID = localID state, proposalValue = 0}}
    putStrLn $ "Final value: " ++ show (proposalNumber state)
    putMVar config newState



