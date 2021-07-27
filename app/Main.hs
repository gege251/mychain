{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Block (BlockchainState (..), appendBlock)
import Blockheader (Blockheader (..), Headerchain (..))
import Data.Time.Clock.POSIX (getPOSIXTime)
import MerkleTree (MTree (..), mkMerkleTree)
import Relude
import Relude.Extra.Map as Map
import Text.Pretty.Simple (pPrint)
import Tx (Tx (..), TxInput (..), TxWithId (..), UTxO (..), UTxOSet (..), mkTxWithId)
import Utils (sha256)

difficulty :: Integer
difficulty = 1e72

genesisHeader :: Headerchain
genesisHeader =
  Cons
    344415770509062919420042971753096059265185458253546776236905952804678601
    ( Blockheader
        { bhMerkleRoot = 573752597514723069550837218413816788272227841783833478352734104569604474114
        , bhPrevHash = 0
        , bhTime = 0
        , bhNonce = 5448
        }
    )
    Nil

genesisState :: BlockchainState
genesisState =
  let coinbase =
        CoinBase
          { seqId = 0
          , txOutputs = UTxO 0 0 :| []
          }
   in BlockchainState
        { bsHeaderChain = genesisHeader
        , bsBlocks =
            fromList
              [
                ( 573752597514723069550837218413816788272227841783833478352734104569604474114
                ,
                  ( MLeaf 573752597514723069550837218413816788272227841783833478352734104569604474114
                  , TxWithId
                      ( 573752597514723069550837218413816788272227841783833478352734104569604474114
                      , CoinBase
                          { seqId = 0
                          , txOutputs = UTxO {utxoPubKeyHash = 0, utxoValue = 10} :| []
                          }
                      )
                      :| []
                  )
                )
              ]
        , bsUTxOSet =
            UTxOSet $
              Map.insert
                (573752597514723069550837218413816788272227841783833478352734104569604474114, 0)
                (UTxO {utxoPubKeyHash = 0, utxoValue = 10})
                mempty
        }

main :: IO ()
main = do
  let pkh1 = 111
  let pkh2 = 222
  b1 <- appendBlock' [] pkh1 genesisState

  let tx1 =
        Tx
          ( TxInput 37254559713211582939483130692192002694443009873178671040163264979314018317130 0 pkh1
              :| []
          )
          (UTxO pkh2 9 :| [])
  let tx2 =
        Tx
          ( TxInput 89649316556255882822091218746644268797576243152683787986346667044098340373621 0 pkh2
              :| []
          )
          (UTxO pkh1 9 :| [])

  b2 <- appendBlock' [tx1, tx2] pkh1 b1

  let tx3 =
        Tx
          ( TxInput 90292656052293902084671052952337440634775277417637406207857298924106690368271 0 pkh1
              :| [ TxInput 114462397245104650884493091832614768601059572004498022648568562135873548415983 0 pkh1
                 ]
          )
          (UTxO pkh2 15 :| [UTxO pkh1 3])
  b3 <- appendBlock' [tx3] pkh1 b2

  pPrint b3
  where
    appendBlock' txs minerPkh lastState = do
      time <- floor . toRational <$> getPOSIXTime
      return $ handleError $ appendBlock difficulty time txs minerPkh lastState

    difficulty = 1e72

    handleError (Left err) = error (toText err)
    handleError (Right x) = x
