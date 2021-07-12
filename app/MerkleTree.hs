{-# LANGUAGE NoImplicitPrelude #-}

module MerkleTree where

import Numeric (readHex, showHex)
import Relude
import Utils (sha256)

-- | Merkle tree
data MTree
  = MNode Integer MTree MTree
  | MLeaf Integer
  deriving (Show)

-- | Partial Merkle tree, consisting only of the minimum set of nodes for verifying a specific
-- transaction ID in the tree
data PMTree
  = PMNode Integer PMTree Integer
  | PMLeaf Integer
  deriving (Show)

-- | Build a Merklee tree from a list of hashes
mkMerkleTree :: NonEmpty Integer -> MTree
mkMerkleTree txIds = mkTree $ fmap MLeaf txIds
  where
    mkTree :: NonEmpty MTree -> MTree
    mkTree (x :| []) = x
    mkTree xss = mkTree $ mkNodes xss

    mkNodes :: NonEmpty MTree -> NonEmpty MTree
    mkNodes (x :| []) = x :| []
    mkNodes (l :| [r]) = MNode (hashPair (nodeHash l) (nodeHash r)) l r :| []
    mkNodes (l :| r : x : xs) =
      MNode (hashPair (nodeHash l) (nodeHash r)) l r
        :| toList (mkNodes (x :| xs))

-- | Convert a Merklee tree into a Partial Merkle tree for a specific leaf
mkPartialMerkleTree :: Integer -> MTree -> Maybe PMTree
mkPartialMerkleTree hash (MLeaf h) | h == hash = Just (PMLeaf h)
mkPartialMerkleTree hash (MLeaf _) = Nothing
mkPartialMerkleTree hash (MNode h l r) =
  case (mkPartialMerkleTree hash l, mkPartialMerkleTree hash r) of
    (Just l, _) -> Just $ PMNode h l (nodeHash r)
    (_, Just r) -> Just $ PMNode h r (nodeHash l)
    _ -> Nothing

verifyMerkleTree :: MTree -> Bool
verifyMerkleTree (MLeaf _) = True
verifyMerkleTree (MNode hash l r) =
  hashPair (nodeHash l) (nodeHash r) == hash
    && verifyMerkleTree l
    && verifyMerkleTree r

-- | Verifies if the given transaction ID is included in the merklee root
verifyPartialMerkleTree :: Integer -> PMTree -> Bool
verifyPartialMerkleTree txId (PMLeaf hash) = hash == txId
verifyPartialMerkleTree txId (PMNode hash l r) =
  hashPair (nodeHash' l) r == hash
    && verifyPartialMerkleTree txId l

-- | Concatenates and hashes two hash numbers (used for Merkle trees)
hashPair :: Integer -> Integer -> Integer
hashPair x y | x > y = hashPair y x
hashPair x y = sha256 $ encodeUtf8 $ showHex x "" <> showHex y ""

nodeHash (MLeaf hash) = hash
nodeHash (MNode hash _ _) = hash

nodeHash' (PMLeaf hash) = hash
nodeHash' (PMNode hash _ _) = hash
