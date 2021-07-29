{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Tx where

import MerkleTree (MTree)
import qualified MerkleTree
import Relude
import Relude.Extra.Map as Map
import Utils (sha256)

-- TxInput is a pair of a transactionId and the index of the utxo inside the transaction
data TxInput = TxInput
  { txInId :: Integer
  , txInUtxoIndex :: Int
  , txInScriptSig :: Integer -- For simplicty sake, this will be the public key hash
  }
  deriving (Show)

newtype TxWithId = TxWithId {getTxWithId :: (Integer, Tx)} deriving (Show)

data Tx
  = CoinBase
      { seqId :: Int
      , txOutputs :: NonEmpty UTxO
      }
  | Tx
      { txInputs :: NonEmpty TxInput
      , txOutputs :: NonEmpty UTxO
      }
  deriving (Show)

isTx :: Tx -> Bool
isTx (Tx _ _) = True
isTx _ = False

isCoinBase :: Tx -> Bool
isCoinBase (CoinBase _ _) = True
isCoinBase _ = False

data UTxO = UTxO
  { utxoPubKeyHash :: Integer
  , utxoValue :: Int
  }
  deriving (Show)

newtype UTxOSet = UTxOSet {getUTxOSet :: Map (Integer, Int) UTxO} deriving (Show, Semigroup)

mkTxId :: Tx -> Integer
mkTxId = sha256 . show

mkTxWithId :: Tx -> TxWithId
mkTxWithId tx = TxWithId (mkTxId tx, tx)

-- | Verifying the validity of a transaction
-- This functions only checks the current transaction, without recursively checking previous ones.
verifyTx :: UTxOSet -> Tx -> Either Text (UTxOSet, Int)
verifyTx utxoSet tx@(CoinBase _ _) = Right (appendToUtxoSet tx utxoSet, 0)
verifyTx utxoSet tx@Tx {txInputs = txIns, txOutputs = txOuts} = do
  (inputUtxos, updatedUtxoSet) <- findInputUtxos utxoSet txIns
  let inputSum = sum $ map utxoValue inputUtxos
  let outputSum = sum $ map utxoValue $ toList txOuts

  let fees = inputSum - outputSum

  if fees < 0
    then Left "inputs are not covering the outputs"
    else Right (appendToUtxoSet tx updatedUtxoSet, fees)

appendToUtxoSet :: Tx -> UTxOSet -> UTxOSet
appendToUtxoSet tx utxoSet =
  let txId = mkTxId tx
      newUtxos = UTxOSet $ fromList $ zip (map (txId,) [0 ..]) (toList (txOutputs tx))
   in utxoSet <> newUtxos

findInputUtxos :: UTxOSet -> NonEmpty TxInput -> Either Text ([UTxO], UTxOSet)
findInputUtxos utxoSet txIns =
  foldlM
    ( \(utxos, UTxOSet utxoSet') TxInput {txInId, txInUtxoIndex, txInScriptSig} -> do
        utxo <-
          Map.lookup (txInId, txInUtxoIndex) utxoSet'
            & maybeToRight ("input utxo not found or used: " <> show txInId <> "#" <> show txInUtxoIndex)

        let updatedUtxoSet = UTxOSet $ Map.delete (txInId, txInUtxoIndex) utxoSet'

        if utxoPubKeyHash utxo == txInScriptSig
          then Right (utxo : utxos, updatedUtxoSet)
          else Left "signature doesn't match the public key hash"
    )
    ([], utxoSet)
    (toList txIns)

mkMerkleTree :: NonEmpty Tx -> MTree
mkMerkleTree txs = MerkleTree.mkMerkleTree $ mkTxId <$> txs
