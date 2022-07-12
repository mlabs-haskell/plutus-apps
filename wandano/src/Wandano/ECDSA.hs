{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Module: Wandano.ECDSA
-- Maintainer: Koz Ross <koz@mlabs.city>
-- Stability: Experimental
-- Portability: GHC only
-- Copyright: (C) 2022 MLabs
--
-- Provides a deserialization \'bridge\' between the external representations of
-- ECDSA verification keys and signatures, and the internal representations of
-- these same things. The former are used for interchange, but the latter are
-- required by Plutus primitive operations.
module Wandano.ECDSA
  ( -- * Types
    VerificationKey (VerificationRawBytes),
    Signature (SignatureRawBytes),

    -- * Functions
    decodeVerificationKey,
    decodeSignature,
  )
where

import Crypto.Secp256k1 (importPubKey, importSig)
import Data.ByteString (ByteString)
import Data.Serialize (put, runPut)

-- | The internal state corresponding to a verification key serialized as DER.
--
-- @since 1.0.0
newtype VerificationKey = VerificationKey ByteString
  deriving
    ( -- | @since 1.0.0
      Eq
    )
    via ByteString
  deriving stock
    ( -- | @since 1.0.0
      Show
    )

-- | Obtain the raw byte data inside a 'VerificationKey', suitable for passing
-- to Plutus primitives.
--
-- This is a read-only pattern: you can pattern match on this, but not construct
-- values with it.
--
-- @since 1.0.0
pattern VerificationRawBytes :: ByteString -> VerificationKey
pattern VerificationRawBytes bs <- VerificationKey bs

-- | Given 'ByteString' data corresponding to a DER-encoded verification
-- (public) key, attempt to construct a 'VerificationKey'. Gives 'Nothing' if
-- the data does not correspond.
--
-- @since 1.0.0
decodeVerificationKey :: ByteString -> Maybe VerificationKey
decodeVerificationKey bs = VerificationKey . runPut . put <$> importPubKey bs

-- | The internal state corresponding to a signature serialized as DER.
--
-- @since 1.0.0
newtype Signature = Signature ByteString
  deriving
    ( -- | @since 1.0.0
      Eq
    )
    via ByteString
  deriving stock
    ( -- | @since 1.0.0
      Show
    )

-- | Obtain the raw byte data inside a 'Signature', suitable for passing to
-- Plutus primitives.
--
-- This is a read-only pattern: you can pattern match on this, but not construct
-- values with it.
--
-- @since 1.0.0
pattern SignatureRawBytes :: ByteString -> Signature
pattern SignatureRawBytes bs <- Signature bs

-- | Given 'ByteString' data corresponding to a DER-encoded signature, attempt
-- to construct a 'Signature'. Gives 'Nothing' if the data does not correspond.
--
-- @since 1.0.0
decodeSignature :: ByteString -> Maybe Signature
decodeSignature bs = Signature . runPut . put <$> importSig bs
