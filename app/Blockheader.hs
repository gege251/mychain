{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Blockheader where

import Relude
import Utils (sha256)

data Blockheader = Blockheader
  { bhMerkleRoot :: Integer
  , bhPrevHash :: Integer
  , bhTime :: Int
  , bhNonce :: Int
  }
  deriving (Show)

data Headerchain
  = Nil
  | Cons Integer Blockheader Headerchain
  deriving (Show)

doWork :: Integer -> (Int -> Blockheader) -> Int -> (Integer, Blockheader)
doWork difficulty mkHeader nonce =
  let headerAttempt = mkHeader nonce
   in case sha256 (show headerAttempt) of
        blockId
          | blockId < difficulty ->
            (blockId, headerAttempt)
        _ ->
          doWork difficulty mkHeader (nonce + 1)

appendHeader :: Integer -> Integer -> Int -> Headerchain -> Headerchain
appendHeader _ _ _ Nil = Nil
appendHeader difficulty merkleRoot time chain@(Cons prevHash _ _) =
  let (blockId, header) = doWork difficulty (Blockheader merkleRoot prevHash time) 0
   in Cons blockId header chain

verifyHeaders :: Integer -> Headerchain -> Bool
verifyHeaders _ Nil = True
verifyHeaders difficulty (Cons blockId header chain) =
  blockId < difficulty
    && sha256 (show header) == blockId
    && verifyHeaders difficulty chain
