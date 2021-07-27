{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Block where

import Blockheader (Headerchain, appendHeader)
import MerkleTree (MTree, getMerkleRoot)
import Relude
import Relude.Extra.Map as Map
import Tx (Tx (..), TxWithId, UTxO (..), UTxOSet, isCoinBase, mkMerkleTree, mkTxWithId, verifyTx)

data BlockchainState = BlockchainState
  { bsHeaderChain :: Headerchain,
    bsBlocks :: Map Integer (MTree, NonEmpty TxWithId),
    bsUTxOSet :: UTxOSet
  }
  deriving (Show)

appendBlock :: Integer -> Int -> [Tx] -> Integer -> BlockchainState -> Either String BlockchainState
appendBlock difficulty time txs pkh BlockchainState {bsHeaderChain, bsBlocks, bsUTxOSet}
  | any isCoinBase txs = Left "there can only be one coinbase transaction in a block"
  | otherwise = do
    updatedUtxoSet <- foldlM verifyTx bsUTxOSet blockTxs

    return $
      BlockchainState
        { bsHeaderChain = updatedHeaderChain,
          bsBlocks = Map.insert merkleRoot (merkleTree, txsWithIds) bsBlocks,
          bsUTxOSet = updatedUtxoSet
        }
  where
    coinbase = CoinBase {txOutputs = UTxO pkh 10 :| []}
    blockTxs = coinbase :| txs
    merkleTree = mkMerkleTree blockTxs
    merkleRoot = getMerkleRoot merkleTree
    updatedHeaderChain = appendHeader difficulty merkleRoot time bsHeaderChain
    txsWithIds = fmap mkTxWithId blockTxs
    allTxs = toList txsWithIds <> concatMap (toList . snd) (toList bsBlocks)
