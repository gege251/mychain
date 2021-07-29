{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Block where

import Blockheader (Blockheader (..), Headerchain (..), appendHeader, chainToList, verifyHeaders)
import MerkleTree (MTree, getLeaves, getMerkleRoot, verifyMerkleTree)
import Relude
import Relude.Extra.Map as Map
import Tx
  ( Tx (..),
    TxWithId (..),
    UTxO (..),
    UTxOSet (..),
    appendToUtxoSet,
    isCoinBase,
    mkMerkleTree,
    mkTxWithId,
    verifyTx,
  )

data BlockchainState = BlockchainState
  { bsHeaderChain :: Headerchain
  , bsBlocks :: Map Integer (MTree, NonEmpty TxWithId)
  , bsUTxOSet :: UTxOSet
  }
  deriving (Show)

generatedCoins = 10

appendBlock :: Integer -> Int -> [Tx] -> Integer -> BlockchainState -> Either Text BlockchainState
appendBlock difficulty time txs minerPkh BlockchainState {bsHeaderChain, bsBlocks, bsUTxOSet}
  | any isCoinBase txs = Left "there can only be one coinbase transaction in a block"
  | otherwise = do
    (updatedUtxoSet, fees) <- foldlM verifyTx' (bsUTxOSet, 0) txs

    prevSeqId <- findPrevSeqId bsHeaderChain bsBlocks

    let coinbase =
          CoinBase
            { seqId = prevSeqId + 1
            , txOutputs = UTxO minerPkh (generatedCoins + fees) :| []
            }
    let blockTxs = coinbase :| txs
    let updatedUtxoSetWithCoinbase = appendToUtxoSet coinbase updatedUtxoSet

    let merkleTree = mkMerkleTree blockTxs
    let merkleRoot = getMerkleRoot merkleTree

    let updatedHeaderChain = appendHeader difficulty merkleRoot time bsHeaderChain
    let txsWithIds = fmap mkTxWithId blockTxs
    let allTxs = toList txsWithIds <> concatMap (toList . snd) (toList bsBlocks)

    return $
      BlockchainState
        { bsHeaderChain = updatedHeaderChain
        , bsBlocks = Map.insert merkleRoot (merkleTree, txsWithIds) bsBlocks
        , bsUTxOSet = updatedUtxoSetWithCoinbase
        }
  where
    verifyTx' (utxoSet, fees) tx = second (+ fees) <$> verifyTx utxoSet tx

findPrevSeqId :: Headerchain -> Map Integer (MTree, NonEmpty TxWithId) -> Either Text Int
findPrevSeqId Nil blocks = Left "no genesis block found"
findPrevSeqId (Cons _ Blockheader {bhMerkleRoot} _) blocks = do
  lastBlock <- maybeToRight "previous block not found " $ Map.lookup bhMerkleRoot blocks
  let lastTsx = map (snd . getTxWithId) $ toList $ snd lastBlock

  case find isCoinBase lastTsx of
    Just CoinBase {seqId} -> Right seqId
    _ -> Left "previous coinbase not found"

-- | Verifies the validity of the transactions against the Merkle Tree, but doesn't
-- cares about the validity of the transactions themselves
verifyBlock :: (MTree, NonEmpty TxWithId) -> Bool
verifyBlock (merkleTree, txs) =
  isTreeValid && treeLeaves == txIds
  where
    isTreeValid = verifyMerkleTree merkleTree
    txIds :: Set Integer = fromList $ map (fst . getTxWithId) $ toList txs
    treeLeaves :: Set Integer = fromList $ getLeaves merkleTree

verifyChain :: Integer -> BlockchainState -> Bool
verifyChain difficulty bs@BlockchainState {bsHeaderChain, bsBlocks} =
  areHeadersValid && areBlocksValid && areTransactionsValid && merkleRoots == merkleRoots'
  where
    areHeadersValid = verifyHeaders difficulty bsHeaderChain
    areBlocksValid = all verifyBlock bsBlocks
    areTransactionsValid = verifyAllTransactions bs
    merkleRoots :: Set Integer = fromList $ Map.keys bsBlocks
    merkleRoots' :: Set Integer = fromList $ map bhMerkleRoot $ chainToList bsHeaderChain

verifyAllTransactions :: BlockchainState -> Bool
verifyAllTransactions BlockchainState {bsBlocks, bsHeaderChain} =
  let merkleRoots = map bhMerkleRoot $ reverse $ chainToList bsHeaderChain
      maybeBlocks = mapM (`Map.lookup` bsBlocks) merkleRoots
   in case maybeBlocks of
        Nothing -> False
        Just blocks ->
          let txs = map (snd . getTxWithId) $ concatMap (toList . snd) blocks
              utxoSet = UTxOSet mempty
           in case foldlM (\us tx -> fst <$> verifyTx us tx) utxoSet txs of
                Left e -> error e
                Right updatedUtxoSet -> do
                  True
