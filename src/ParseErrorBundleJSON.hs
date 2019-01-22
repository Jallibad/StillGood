{-# OPTIONS_GHC -fno-warn-orphans #-}

module ParseErrorBundleJSON where

import Data.Aeson
import Text.Megaparsec

instance (Stream s, ShowErrorComponent e) => ToJSON (ParseErrorBundle s e) where
	toJSON = toJSON . errorBundlePretty