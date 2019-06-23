{-# OPTIONS_GHC -fno-warn-orphans #-}

module Replica.Internal where

import qualified Data.ByteString            as B
import qualified Data.FileEmbed             as FE

import           Language.Haskell.TH.Syntax (Lift, lift)

instance Lift B.ByteString where
  lift = FE.bsToExp

replace :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
replace needle with str
  | Just suffix <- B.stripPrefix needle suffix' = prefix <> with <> suffix
  | otherwise = error "Can't find substring"
  where
    (prefix, suffix') = B.breakSubstring needle str
