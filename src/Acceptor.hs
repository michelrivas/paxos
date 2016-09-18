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
--| checkProposal
checkProposal :: ServerState -> Message -> (ServerState, Maybe String)
checkProposal state msg = do
    let prop = highestProposal state
    let newState = state {proposalNumber = messageValue msg, highestProposal = Proposal {proposalID = messageId msg, proposalValue = messageValue msg}}
    case compare (messageValue msg) (proposalValue prop) of 
        LT -> (state, Nothing)
        _  -> (newState, Just $ ("2:" ++) . show $ proposalNumber state)

--| checkAccept
checkAccept :: ServerState -> Message -> (ServerState, Maybe String)
checkAccept state msg = do
    let prop = proposalNumber state
    let newState = state {proposalNumber = messageValue msg, highestProposal = Proposal {proposalID = messageId msg, proposalValue = messageValue msg}}
    case compare (messageValue msg) (prop) of 
        EQ -> (newState, Just $ ("4:" ++) . show $ proposalNumber state)
        _  -> (state, Nothing)

--| valueDecided
valueDecided :: ServerState -> Message -> ServerState
valueDecided state msg = state {proposalNumber = 0, highestProposal = Proposal {proposalID = localID state, proposalValue = messageValue msg}}



