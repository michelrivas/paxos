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
    acceptAccepted
) where

import Control.Concurrent.MVar
--import Control.Concurrent (threadDelay)

import Utils

-- PROPOSER
prepareRequest :: String -> String
prepareRequest value = "1:" ++ value

prepareAccepted :: ServerState -> Message -> (ServerState, Maybe String)
prepareAccepted state msg = do
    let quorum = (prepareQuorum state) + 1
--  Compare new value with proposed value
    let newState = state {prepareQuorum = quorum, highestProposal = Proposal {proposalID = messageId msg, proposalValue = messageValue msg}}
    let majority = length (serverList state) `quot` 2 + 1
    case compare quorum majority of
        LT -> (newState, Nothing)
        _  -> (newState, Just $ ("3:" ++) . show $ proposalNumber state)

acceptAccepted :: ServerState -> Message -> (ServerState, Maybe String)
acceptAccepted state msg = do
    let quorum = (acceptQuorum state) + 1
    let newState = state {acceptQuorum = quorum}
    let majority = length (serverList state) `quot` 2 + 1
    case compare quorum majority of
        LT -> (newState, Nothing)
        _  -> (newState, Just $ ("5:" ++) . show $ proposalNumber state)




