{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Block where

import Blockheader (Headerchain, appendHeader)
import MerkleTree (MTree, getMerkleRoot)
import Relude
import Relude.Extra.Map as Map
import Tx (Tx, TxWithId, mkMerkleTree, mkTxWithId, verifyTx)

data BlockchainState = BlockchainState
  { bsHeaderChain :: Headerchain,
    bsBlocks :: Map Integer (MTree, NonEmpty TxWithId)
  }

appendBlock :: Integer -> Int -> NonEmpty Tx -> BlockchainState -> Maybe BlockchainState
appendBlock difficulty time txs BlockchainState {bsHeaderChain, bsBlocks} =
  if isValid
    then
      Just $
        BlockchainState
          { bsHeaderChain = updatedHeaderChain,
            bsBlocks = Map.insert merkleRoot (merkleTree, txsWithIds) bsBlocks
          }
    else Nothing
  where
    merkleTree = mkMerkleTree txs
    merkleRoot = getMerkleRoot merkleTree
    updatedHeaderChain = appendHeader difficulty merkleRoot time bsHeaderChain
    txsWithIds = fmap mkTxWithId txs
    allTxs = toList txsWithIds <> concatMap (toList . snd) (toList bsBlocks)
    isValid = all (verifyTx allTxs) txs
