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

checkAccept :: ServerState -> Message -> (ServerState, Maybe String)
checkAccept state msg = do
    let prop = proposalNumber state
    --putMVar config state
    let newState = state {proposalNumber = messageValue msg, highestProposal = Proposal {proposalID = messageId msg, proposalValue = messageValue msg}}
    case compare (messageValue msg) (prop) of 
        EQ -> (newState, Just $ ("4:" ++) . show $ proposalNumber state)
        _  -> (state, Nothing)

valueDecided :: MVar ServerState -> Server -> Message -> IO ()
valueDecided config server msg = do
--    threadDelay 5000000
    state <- takeMVar config
    let newState = state {proposalNumber = 0, highestProposal = Proposal {proposalID = localID state, proposalValue = 0}}
    putStrLn $ "Final value: " ++ show (proposalNumber state)
    putMVar config newState



