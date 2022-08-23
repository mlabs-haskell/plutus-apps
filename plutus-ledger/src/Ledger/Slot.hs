{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
-- Otherwise we get a complaint about the 'fromIntegral' call in the generated instance of 'Integral' for 'Ada'
{-# OPTIONS_GHC -Wno-identities #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable   #-}

-- | Slots and slot ranges.
module Ledger.Slot(
      Slot(..)
    , SlotRange
    , width
    ) where

import Codec.Serialise.Class (Serialise)
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Prelude qualified as Haskell
import Prettyprinter (Pretty (pretty), (<+>))


import PlutusTx qualified
import PlutusTx.Lift (makeLift)
import PlutusTx.Prelude

import Data.Data (Data)
import Plutus.V1.Ledger.Interval

{- HLINT ignore "Redundant if" -}

-- | The slot number. This is a good proxy for time, since on the Cardano blockchain
-- slots pass at a constant rate.
newtype Slot = Slot { getSlot :: Integer }
    deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic, Data)
    deriving anyclass (FromJSON, FromJSONKey, ToJSON, ToJSONKey, NFData)
    deriving newtype (AdditiveSemigroup, AdditiveMonoid, AdditiveGroup, Eq, Ord, Enum, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
    deriving newtype (Haskell.Num, Haskell.Enum, Haskell.Real, Haskell.Integral, Serialise, Hashable)

makeLift ''Slot

instance Pretty Slot where
    pretty (Slot i) = "Slot" <+> pretty i

-- | An 'Interval' of 'Slot's.
type SlotRange = Interval Slot

{-# INLINABLE width #-}
-- | Number of 'Slot's covered by the interval, if finite. @width (from x) == Nothing@.
width :: SlotRange -> Maybe Integer
width (Interval (LowerBound (Finite (Slot s1)) in1) (UpperBound (Finite (Slot s2)) in2)) =
    let lowestValue = if in1 then s1 else s1 + 1
        highestValue = if in2 then s2 else s2 - 1
    in if lowestValue <= highestValue
    -- +1 avoids fencepost error: width of [2,4] is 3.
    then Just $ (highestValue - lowestValue) + 1
    -- low > high, i.e. empty interval
    else Nothing
-- Infinity is involved!
width _ = Nothing


deriving anyclass instance (Hashable a) => Hashable (Interval a)
deriving anyclass instance (Serialise a) => Serialise (Interval a)
deriving anyclass instance (ToJSON a) => ToJSON (Interval a)
deriving anyclass instance (FromJSON a) => FromJSON (Interval a)

deriving anyclass instance (Hashable a) => Hashable (LowerBound a)
deriving anyclass instance (Serialise a) => Serialise (LowerBound a)
deriving anyclass instance (ToJSON a) => ToJSON (LowerBound a)
deriving anyclass instance (FromJSON a) => FromJSON (LowerBound a)

deriving anyclass instance (Hashable a) => Hashable (UpperBound a)
deriving anyclass instance (Serialise a) => Serialise (UpperBound a)
deriving anyclass instance (ToJSON a) => ToJSON (UpperBound a)
deriving anyclass instance (FromJSON a) => FromJSON (UpperBound a)

deriving anyclass instance (Hashable a) => Hashable (Extended a)
deriving anyclass instance (Serialise a) => Serialise (Extended a)
deriving anyclass instance (ToJSON a) => ToJSON (Extended a)
deriving anyclass instance (FromJSON a) => FromJSON (Extended a)
