{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Spec where

import Blockheader (Headerchain)
import qualified Blockheader
import qualified Main
import qualified MerkleTree
import Relude hiding (head)
import Relude.Unsafe (fromJust, head)
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
      QC.testProperty "Merklee tree verification" $
        \merkleTree -> MerkleTree.verifyMerkleTree merkleTree,
      QC.testProperty "Partial Merklee tree verification" $
        \(PartialTreeWithTxId (partialMerkleTree, txId)) ->
          MerkleTree.verifyPartialMerkleTree txId partialMerkleTree,
      QC.testProperty "Tx verification" $
        \(Txs txs) ->
          Tx.verifyTx txs (head txs)
    ]

instance Arbitrary Headerchain where
  arbitrary = do
    let d = 1e75 -- difficulty
    merkleRoots :: [Integer] <- arbitrary
    times :: [Int] <- arbitrary

    return $
      foldl'
        (\prev (mr, t) -> Blockheader.appendHeader d mr t prev)
        Main.genesis
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

newtype Txs = Txs {getTxs :: [Tx.Tx]} deriving (Show)

instance Arbitrary Txs where
  arbitrary = do
    pubKeyHashes :: [Integer] <- repeat <$> elements [1, 2, 3, 4]
    values :: [Int] <- getNonNegative <<$>> arbitrary

    return $
      Txs $
        scanl
          (\prevTx (pkh, v) -> Tx.Tx (Tx.txOutputs prevTx) (Tx.UTxO pkh v :| []))
          (Tx.CoinBase (Tx.UTxO (head pubKeyHashes) 1000 :| []))
          $ zip pubKeyHashes values
