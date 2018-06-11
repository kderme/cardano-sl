{-# LANGUAGE OverloadedStrings #-}

module Test.Pos.Infra.Gen
        (
        -- DHT Generators
          genDataMsg
        , genInvMsg
        , genMempoolMsg
        , genReqMsg
        , genResMsg
        , genDHTData
        , genDHTKey

        -- Slotting Generators
        , genEpochSlottingData
        , genSlottingData
        ) where

import           Universum

import           Data.Time.Units (Millisecond, Microsecond,
                                  fromMicroseconds)
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Network.Kademlia.HashNodeId (genNonce, hashAddress)

import           Pos.Core (EpochIndex (..), TimeDiff (..))
import           Pos.Crypto.Random (deterministic)
import           Pos.Infra.Communication.Types.Relay (DataMsg (..),
                                                      InvMsg (..),
                                                      MempoolMsg (..),
                                                      ReqMsg (..),
                                                      ResMsg (..))
import           Pos.Infra.DHT (DHTData (..), DHTKey (..))
import           Pos.Infra.Slotting.Types (EpochSlottingData (..),
                                           SlottingData,
                                           createSlottingDataUnsafe)

----------------------------------------------------------------------------
-- DHT Generators
----------------------------------------------------------------------------

genInvMsg :: Gen a -> Gen (InvMsg a)
genInvMsg genA = InvMsg <$> genA

genReqMsg :: Gen (Maybe a) -> Gen (ReqMsg a)
genReqMsg genMA = ReqMsg <$> genMA

genResMsg :: Gen a -> Gen (ResMsg a)
genResMsg genA = ResMsg <$> genA <*> Gen.bool

genMempoolMsg :: Gen (MempoolMsg a)
genMempoolMsg = pure MempoolMsg

genDataMsg :: Gen a -> Gen (DataMsg a)
genDataMsg genA = DataMsg <$> genA

genDHTKey :: Gen DHTKey
genDHTKey = pure $ DHTKey $ hashAddress $ deterministic "nonce" genNonce

genDHTData :: Gen DHTData
genDHTData = pure $ DHTData ()

----------------------------------------------------------------------------
-- Slotting Generators
----------------------------------------------------------------------------

genEpochSlottingData :: Gen EpochSlottingData
genEpochSlottingData = EpochSlottingData <$> genMillisecond <*> genTimeDiff

genSlottingData :: Gen SlottingData
genSlottingData = createSlottingDataUnsafe <$> genMap
  where
    genMap :: Gen (Map EpochIndex EpochSlottingData)
    genMap = Gen.map Range.constantBounded genEpochIndexDataPair

genEpochIndexDataPair :: Gen (EpochIndex, EpochSlottingData)
genEpochIndexDataPair = do
    i <- genEpochIndex
    sd <- genEpochSlottingData
    pure (i, sd)

----------------------------------------------------------------------------
-- Helper Generators
----------------------------------------------------------------------------

genMillisecond :: Gen Millisecond
genMillisecond =
    fromMicroseconds <$> (toInteger <$> Gen.int Range.constantBounded)

genMicrosecond :: Gen Microsecond
genMicrosecond =
    fromMicroseconds <$> (toInteger <$> Gen.int Range.constantBounded)

-- TimeDiff is from core so this generator will be moved eventually.
genTimeDiff :: Gen TimeDiff
genTimeDiff = TimeDiff <$> genMicrosecond

-- EpochIndex is from core so this generator will be moved eventually.
genEpochIndex :: Gen EpochIndex
genEpochIndex = EpochIndex <$> Gen.word64 Range.constantBounded
