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
import Relude hiding (head, last, tail)
import Relude.Unsafe (fromJust, head, last, tail)
import Test.Tasty
import Test.Tasty.QuickCheck (arbitrary, elements)
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
           in isRight $ Block.appendBlock difficulty 0 txs pkh Main.genesisState,
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
    pubKeyHashes :: [Integer] <- repeat <$> elements [1, 2, 3, 4]
    values :: [Int] <- getNonNegative <<$>> getNonEmpty <$> arbitrary

    let sortedValues = reverse (sort values)

    return $
      zip pubKeyHashes (tail sortedValues)
        & scanl
          ( \(Tx.TxWithId (prevTxId, prevTx)) (pkh, v) ->
              let prevOutputLength = length $ Tx.txOutputs prevTx
                  inputs =
                    ( \i ->
                        Tx.TxInput
                          { Tx.txInId = prevTxId,
                            Tx.txInUtxoIndex = i,
                            Tx.txInScriptSig = pkh
                          }
                    )
                      <$> (0 :| [1 .. (prevOutputLength - 1)])
                  outputs = (Tx.UTxO pkh v :| [])
                  nextTx = Tx.Tx {Tx.txInputs = inputs, Tx.txOutputs = outputs}
               in Tx.mkTxWithId nextTx
          )
          (Tx.mkTxWithId (Tx.CoinBase (Tx.UTxO (head pubKeyHashes) (head sortedValues) :| [])))
        & Txs
