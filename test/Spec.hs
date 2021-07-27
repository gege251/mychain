{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Spec where

import qualified Block
import Blockheader (Headerchain)
import qualified Blockheader
import qualified Main
import qualified MerkleTree
import Relude hiding (head, last)
import Relude.Unsafe (fromJust, head, last)
import Test.Tasty
import Test.Tasty.QuickCheck (arbitrary, choose, elements)
import Test.Tasty.QuickCheck as QC
import qualified Tx

main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ QC.testProperty "Blockchain headers verification" $
        \headerChain ->
          let difficulty = 1e75
           in Blockheader.verifyHeaders difficulty headerChain,
      QC.testProperty "Appending block and verifying transactions" $
        \(Txs txsWithIds) ->
          let difficulty = 1e75
              pkh = 1234
              txs = map (snd . Tx.getTxWithId) txsWithIds
           in case Block.appendBlock difficulty 0 txs pkh Main.genesisState of
                Right _ -> True
                Left err -> error (toText err),
      QC.testProperty "Merklee tree verification" $
        \merkleTree -> MerkleTree.verifyMerkleTree merkleTree,
      QC.testProperty "Partial Merklee tree verification" $
        \(PartialTreeWithTxId (partialMerkleTree, txId)) ->
          MerkleTree.verifyPartialMerkleTree txId partialMerkleTree
    ]

instance Arbitrary Headerchain where
  arbitrary = do
    let d = 1e75 -- difficulty
    merkleRoots :: [Integer] <- arbitrary
    times :: [Int] <- arbitrary

    return $
      foldl'
        (\prev (mr, t) -> Blockheader.appendHeader d mr t prev)
        Main.genesisHeader
        $ zip merkleRoots times

instance Arbitrary MerkleTree.MTree where
  arbitrary = do
    xss <- getNonNegative <<$>> getNonEmpty <$> arbitrary
    let (x : xs) = xss
    let txIds = x :| xs
    return $ MerkleTree.mkMerkleTree txIds

newtype PartialTreeWithTxId = PartialTreeWithTxId
  { getPartialTreeWithTxId :: (MerkleTree.PMTree, Integer)
  }
  deriving (Show)

instance Arbitrary PartialTreeWithTxId where
  arbitrary = do
    xss <- getNonNegative <<$>> getNonEmpty <$> arbitrary
    let (x : xs) = xss
    let txIds = x :| xs
    txId <- elements xss
    let merkleTree = MerkleTree.mkMerkleTree txIds
    return $ PartialTreeWithTxId (fromJust $ MerkleTree.mkPartialMerkleTree txId merkleTree, txId)

newtype Txs = Txs {getTxs :: [Tx.TxWithId]} deriving (Show)

instance Arbitrary Txs where
  arbitrary = do
    pubKeyHashes :: [Integer] <- repeat <$> elements [0]
    values :: [Int] <- listOf $ chooseInt (0, 10)

    let sortedValues = reverse (sort values)

    return $
      zip pubKeyHashes sortedValues
        & foldl' mkTx []
        & reverse
        & Txs
    where
      mkTx txs (pkh, v) =
        let prevTxId =
              case txs of
                [] -> 573752597514723069550837218413816788272227841783833478352734104569604474114
                Tx.TxWithId (txId, _) : _ -> txId

            inputs =
              Tx.TxInput
                { Tx.txInId = prevTxId,
                  Tx.txInUtxoIndex = 0,
                  Tx.txInScriptSig = pkh
                }
                :| []

            outputs = (Tx.UTxO pkh v :| [])
            nextTx = Tx.Tx {Tx.txInputs = inputs, Tx.txOutputs = outputs}
         in Tx.mkTxWithId nextTx : txs
