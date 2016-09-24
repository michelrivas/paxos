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
-- | Paxos Proposer
--
-----------------------------------------------------------------------------

module Proposer (
    prepareRequest,
    prepareAccepted,
    acceptAccepted
) where

import Control.Concurrent.MVar
--import Control.Concurrent (threadDelay)

import Utils

-- PROPOSER
-- | Phase 1a
-- | The proposer makes a proposal
prepareRequest :: String -> String
prepareRequest value = "1:" ++ value

-- | Phase 2a
-- | The proposer makes an accept request
prepareAccepted :: ServerState -> Message -> (ServerState, Maybe String)
prepareAccepted state msg = do
    let quorum = (prepareQuorum state) + 1
    let newState = state {
        prepareQuorum = quorum, 
        highestProposal = Proposal {
            proposalID = messageId msg, 
            proposalValue = messageValue msg
        }
    }
    case compare (proposalNumber state) (messageValue msg) of
        LT -> (newState, Nothing)
        _  -> case compare quorum (majority state) of
                LT -> (newState, Nothing)
                _  -> (newState, Just $ ("3:" ++) . show $ proposalNumber state)


-- | Phase 2b
-- | The proposer checks the accept response received
acceptAccepted :: ServerState -> Message -> (ServerState, Maybe String)
acceptAccepted state msg = do
    let quorum = (acceptQuorum state) + 1
    let newState = state {acceptQuorum = quorum}
    case compare quorum (majority state) of
        LT -> (newState, Nothing)
        _  -> (newState, Just $ ("5:" ++) . show $ proposalNumber state)




