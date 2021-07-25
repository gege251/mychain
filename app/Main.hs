{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Block (BlockchainState (..), appendBlock)
import Blockheader (Blockheader (..), Headerchain (..))
import MerkleTree (MTree (..), mkMerkleTree)
import Relude
import Relude.Extra.Map as Map
import Tx (Tx (..), TxInput (..), TxWithId (..), UTxO (..), mkTxWithId)
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
              ]
        }

main :: IO ()
main = do
  print "Demo"
  let pkh = 432
  let Just b2 = appendBlock 1e72 0 [] pkh genesisState
  let tx1 =
        Tx
          ( TxInput 78902268065043156483025095612620881616981048192609966680429747496657945621891 0 432
              :| []
          )
          (UTxO 543 9 :| [])
  print $ appendBlock 1e72 1 [tx1] pkh b2
