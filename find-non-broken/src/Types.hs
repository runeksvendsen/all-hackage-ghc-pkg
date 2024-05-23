{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

module Types
  ( NixPackageSet(..)
  ) where

import qualified Data.Text as T

data NixPackageSet = NixPackageSet
  { nixPackageSetNixFile :: FilePath -- ^ .nix file
  , nixPackageSetAttribute :: T.Text
  }
