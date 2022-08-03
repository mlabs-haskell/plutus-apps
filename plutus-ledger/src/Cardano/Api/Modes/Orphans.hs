{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Api.Modes.Orphans where

import Cardano.Api (BabbageEra, CardanoMode, EraInMode (BabbageEraInCardanoMode))
import Data.Aeson (FromJSON (parseJSON), Value)
import Data.Aeson.Types (Parser, prependFailure, typeMismatch)


instance FromJSON (EraInMode BabbageEra CardanoMode) where
  parseJSON "BabbageEraInCardanoMode" = pure BabbageEraInCardanoMode
  parseJSON invalid =
      invalidJSONFailure "BabbageEraInCardanoMode"
                         "parsing 'EraInMode Babbage CardanoMode' failed, "
                         invalid

invalidJSONFailure :: String -> String -> Value -> Parser a
invalidJSONFailure expectedType errorMsg invalidValue =
    prependFailure errorMsg
                   (typeMismatch expectedType invalidValue)
