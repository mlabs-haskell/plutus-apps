{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module Ledger.Constraints.Metadata (
  OtherFields(..),
  NftMetadataFiles(..),
  NftMetadataToken(..),
  NftMetadata(..),
  TxMetadata(..),
  mkNftMetadata,
 ) where

import Cardano.Prelude (decodeUtf8)
import Data.Aeson (ToJSON, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Pair)
import Data.ByteString.Base16 (encode)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Plutus.V1.Ledger.Value (CurrencySymbol, TokenName, unCurrencySymbol, unTokenName)
import PlutusTx.Builtins.Internal (BuiltinByteString (BuiltinByteString))
import Prelude (Eq, Maybe (Just, Nothing), Monoid (mempty), Semigroup ((<>)), Show, ($), (.))

-- | Arbitrary additional fields
newtype OtherFields = OtherFields {getOtherField :: Map Text Aeson.Value}
  deriving newtype (Semigroup, Monoid)
  deriving stock (Show, Eq)

-- | Include user-defined additional fields in JSON object
addOtherFields :: OtherFields -> [Pair] -> [Pair]
addOtherFields (OtherFields fields) xs = (xs <>) $ Map.elems $ Map.mapWithKey (.=) fields

-- | Add field to JSON object when is Just
addFieldWhenJust :: ToJSON v => Text -> Maybe v -> [Pair] -> [Pair]
addFieldWhenJust _ Nothing xs  = xs
addFieldWhenJust k (Just v) xs = (k .= v) : xs

-- | Add list of fields to JSON object when is not empty
addFieldWhenNotEmpty :: ToJSON v => Text -> [v] -> [Pair] -> [Pair]
addFieldWhenNotEmpty _  [] xs = xs
addFieldWhenNotEmpty k v xs   = (k .= v) : xs

data NftMetadataFiles = NftMetadataFiles
  { nmfName        :: Text
  , nmfMediaType   :: Text
  , nmfSrc         :: Text
  , nmfOtherFields :: OtherFields
  }
  deriving stock (Show, Eq)

instance ToJSON NftMetadataFiles where
  toJSON NftMetadataFiles {..} = object $
    addOtherFields nmfOtherFields $
    [ "name" .= nmfName
    , "src" .= nmfSrc
    , "mediaType" .= nmfMediaType
    ]


data NftMetadataToken = NftMetadataToken
  { nmtName        :: Text
  , nmtImage       :: Text
  , nmtMediaType   :: Maybe Text
  , nmtDescription :: Maybe Text
  , nmtFiles       :: [NftMetadataFiles]
  , nmtOtherFields :: OtherFields
  }
  deriving stock (Show, Eq)

instance ToJSON NftMetadataToken where
  toJSON NftMetadataToken {..} = object $
    addOtherFields nmtOtherFields $
    addFieldWhenNotEmpty "files" nmtFiles $
    addFieldWhenJust "mediaType" nmtMediaType $
    addFieldWhenJust "description" nmtDescription $
    [ "name" .= nmtName
    , "image" .= nmtImage
    ]


newtype NftMetadata = NftMetadata
  { nfts :: Map CurrencySymbol (Map TokenName NftMetadataToken)
  }
  deriving stock (Show, Eq)

instance Semigroup NftMetadata where
  m1 <> m2 = NftMetadata { nfts = Map.unionWith Map.union (nfts m1) (nfts m2) }

instance Monoid NftMetadata where
  mempty = NftMetadata mempty

instance ToJSON NftMetadata where
  toJSON NftMetadata {..} =
    object ("version" .= ("1.0" :: Text) : Map.elems (Map.mapWithKey mkJsonPolicy nfts))
    where
      mkJsonPolicy policyId tokens =
        (toHex . unCurrencySymbol $ policyId) .= (object $ Map.elems $ Map.mapWithKey mkJsonToken tokens)
      mkJsonToken assetName token =
        (toText . unTokenName $ assetName) .= token

      toHex :: BuiltinByteString -> Text
      toHex (BuiltinByteString str) = decodeUtf8 (encode str)

      toText :: BuiltinByteString -> Text
      toText (BuiltinByteString str) = decodeUtf8 str


data TxMetadata = TxMetadata
  { txMetadataNft   :: Maybe NftMetadata
  , txMetadataOther :: OtherFields
  }
  deriving stock (Show, Eq)

instance Semigroup TxMetadata where
  m1 <> m2 = TxMetadata
    { txMetadataNft = txMetadataNft m1 <> txMetadataNft m2
    , txMetadataOther = txMetadataOther m1 <> txMetadataOther m2
    }

instance ToJSON TxMetadata where
  toJSON TxMetadata {..} =
    object $
      addFieldWhenJust "721" txMetadataNft $
      addOtherFields txMetadataOther []

-- | Create minimal NFT metadata
mkNftMetadata
  :: CurrencySymbol
  -> TokenName
  -- ^ Actual token name
  -> Text
  -- ^ NFT metadata name
  -> Text
  -- ^ NFT image link
  -> NftMetadata
mkNftMetadata cs tn name image =
  NftMetadata
    $ Map.singleton cs
    $ Map.singleton tn
    $ NftMetadataToken name image Nothing Nothing mempty mempty
