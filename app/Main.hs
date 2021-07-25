{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Block
import Blockheader (Blockheader (..), Headerchain (..))
import MerkleTree (mkMerkleTree)
import Relude
import Tx
import Utils (sha256)

difficulty :: Integer
difficulty = 1e72

genesis :: Headerchain
genesis =
  Cons
    61495029430829067834745655991056131074764960255239822034051742195574945
    ( Blockheader
        { bhMerkleRoot = 0,
          bhPrevHash = 0,
          bhTime = 0,
          bhNonce = 24231
        }
    )
    Nil

main :: IO ()
main = print "Ready"
