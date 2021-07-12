{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Tx where

import MerkleTree (MTree)
import qualified MerkleTree
import Relude
import Utils (sha256)

data Tx
  = CoinBase {txOutputs :: NonEmpty UTxO}
  | Tx
      { txInputs :: NonEmpty UTxO,
        txOutputs :: NonEmpty UTxO
      }
  deriving (Show)

data UTxO = UTxO
  { utxoPubKeyHash :: Integer,
    utxoValue :: Int
  }
  deriving (Show)

verifyTx :: [Tx] -> Tx -> Bool
verifyTx _ (CoinBase _) = True
verifyTx txs tx =
  let inputPubKeyHashes = utxoPubKeyHash <$> txInputs tx
      spentTxs = filter (\outTx -> any (containsAsOutput outTx) inputPubKeyHashes) txs
   in all (verifyTx txs) spentTxs
  where
    containsAsOutput outTx inTx = inTx `elem` (utxoPubKeyHash <$> txOutputs outTx)

mkMerkleTree :: NonEmpty Tx -> MTree
mkMerkleTree txs = MerkleTree.mkMerkleTree $ sha256 . show <$> txs
