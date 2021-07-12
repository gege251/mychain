{-# LANGUAGE NoImplicitPrelude #-}

module Utils where

import Crypto.Hash (Digest, hashWith)
import Crypto.Hash.Algorithms (SHA256 (..))
import Numeric (readHex, showHex)
import Relude hiding (head)
import Relude.Unsafe (head)

sha256 :: ByteString -> Integer
sha256 str = head $ fst <$> readHex (show $ hashWith SHA256 str)
