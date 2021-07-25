{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Tx where

import MerkleTree (MTree)
import qualified MerkleTree
import Relude
import Utils (sha256)

-- TxInput is a pair of a transactionId and the index of the utxo inside the transaction
data TxInput = TxInput
  { txInId :: Integer,
    txInUtxoIndex :: Int,
    txInScriptSig :: Integer -- For simplicty sake, this will be the public key hash
  }
  deriving (Show)

newtype TxWithId = TxWithId {getTxWithId :: (Integer, Tx)} deriving (Show)

data Tx
  = CoinBase {txOutputs :: NonEmpty UTxO}
  | Tx
      { txInputs :: NonEmpty TxInput,
        txOutputs :: NonEmpty UTxO
      }
  deriving (Show)

data UTxO = UTxO
  { utxoPubKeyHash :: Integer,
    utxoValue :: Int
  }
  deriving (Show)

mkTxId :: Tx -> Integer
mkTxId = sha256 . show

mkTxWithId :: Tx -> TxWithId
mkTxWithId tx = TxWithId (mkTxId tx, tx)

-- | Verifying the validity of a transaction
-- This functions only checks the current transaction, without recursively checking previous ones.
verifyTx :: [TxWithId] -> Tx -> Bool
verifyTx _ (CoinBase _) = True
verifyTx prevTxs Tx {txInputs = txIns, txOutputs = txOuts} =
  all isJust inputUtxos -- All input utxos are valid
    && inputSum >= outputSum
  where
    inputUtxos =
      map
        ( \TxInput {txInId, txInUtxoIndex, txInScriptSig} -> do
            TxWithId (_, tx) <-
              find (\(TxWithId (prevTxId, prevTx)) -> prevTxId == txInId) prevTxs
            utxo <- toList (txOutputs tx) !!? txInUtxoIndex
            if utxoPubKeyHash utxo == txInScriptSig
              then Just utxo
              else Nothing
        )
        (toList txIns)
    inputSum = sum $ map utxoValue $ catMaybes inputUtxos
    outputSum = sum $ map utxoValue $ toList txOuts

mkMerkleTree :: NonEmpty Tx -> MTree
mkMerkleTree txs = MerkleTree.mkMerkleTree $ mkTxId <$> txs
