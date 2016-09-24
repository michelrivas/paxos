-----------------------------------------------------------------------------
--
-- Module      :  Acceptor
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :  m.rivas1@uni.brighton.ac.uk
-- Stability   :
-- Portability :
--
-- | Paxos Acceptor
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
-- | Phase 1b
-- | The acceptor checks a proposal
checkProposal :: ServerState -> Message -> (ServerState, Maybe String)
checkProposal state msg = do
    let value = messageValue msg
    let newState = state {
        proposalNumber = value, 
        highestProposal = Proposal {
            proposalID = messageId msg, 
            proposalValue = value
        }
    }
    case compare value (proposalValue $ highestProposal state) of 
        LT -> (state, Nothing)
        _  -> (newState, Just $ "2:" ++ show value)

-- | Phase 2b
-- | The acceptor accepts a proposal
checkAccept :: ServerState -> Message -> (ServerState, Maybe String)
checkAccept state msg = do
    let value = messageValue msg
    let newState = state {
        proposalNumber = value, 
        highestProposal = Proposal {
            proposalID = messageId msg, 
            proposalValue = value
        }
    }
    case compare value (proposalNumber state) of 
        EQ -> (newState, Just $ "4:" ++ show value)
        _  -> (state, Nothing)

-- | Phase 3
-- | The acceptor (in this case acts as learner) learns a value
valueDecided :: ServerState -> Message -> ServerState
valueDecided state msg = state {
    learnedValues = (messageValue msg) : (learnedValues state), 
    proposalNumber = 0, 
    highestProposal = Proposal {
        proposalID = localID state, 
        proposalValue = messageValue msg
    }
}



