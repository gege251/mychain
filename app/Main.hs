{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Block (BlockchainState (..), appendBlock)
import Blockheader (Blockheader (..), Headerchain (..))
import MerkleTree (MTree (..), mkMerkleTree)
import Relude
import Relude.Extra.Map as Map
import Tx (Tx (..), TxInput (..), TxWithId (..), UTxO (..), UTxOSet (..), mkTxWithId)
import Utils (sha256)

difficulty :: Integer
difficulty = 1e72

genesisHeader :: Headerchain
genesisHeader =
  Cons
    640226666735278793971139464131607474047383619547795670366430405319508965
    ( Blockheader
        { bhMerkleRoot = 9035877082247029798048353064123554423368089210468831133597218371229685373943,
          bhPrevHash = 0,
          bhTime = 0,
          bhNonce = 258805
        }
    )
    Nil

genesisState :: BlockchainState
genesisState =
  let coinbase = CoinBase {txOutputs = UTxO 0 0 :| []}
   in BlockchainState
        { bsHeaderChain = genesisHeader,
          bsBlocks =
            fromList
              [ ( 9035877082247029798048353064123554423368089210468831133597218371229685373943,
                  ( MLeaf 9035877082247029798048353064123554423368089210468831133597218371229685373943,
                    TxWithId
                      ( 9035877082247029798048353064123554423368089210468831133597218371229685373943,
                        CoinBase {txOutputs = UTxO {utxoPubKeyHash = 0, utxoValue = 10} :| []}
                      )
                      :| []
                  )
                )
              ],
          bsUTxOSet =
            UTxOSet $
              Map.insert
                (9035877082247029798048353064123554423368089210468831133597218371229685373943, 0)
                (UTxO {utxoPubKeyHash = 0, utxoValue = 10})
                mempty
        }

main :: IO ()
main = do
  print "Demo"
  let pkh1 = 432
  let pkh2 = 543
  let Right b2 = appendBlock 1e72 0 [] pkh1 genesisState
  let tx1 =
        Tx
          ( TxInput 78902268065043156483025095612620881616981048192609966680429747496657945621891 0 pkh1
              :| []
          )
          (UTxO pkh2 9 :| [])
  let tx2 =
        Tx
          ( TxInput 3354073312312241297602561830913683538826344268112606998958799508091882577646 0 pkh2
              :| []
          )
          (UTxO pkh1 9 :| [])
  print $ appendBlock 1e72 1 [tx1, tx2] pkh1 b2
