{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Replica.VDOM
  ( module Replica.VDOM
  , module Replica.VDOM.Types
  , module Replica.VDOM.Diff
  , module Replica.VDOM.Render
  ) where

import qualified Data.ByteString            as B
import qualified Data.FileEmbed             as FE
import           Data.Monoid                ((<>))
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Builder     as TB
import qualified Data.Map                   as M

import           Language.Haskell.TH.Syntax (lift)
import           Replica.Internal           (replace)

import           Replica.VDOM.Types         (HTML, VDOM(VNode,VLeaf,VText), Attrs, Attr(AText,ABool,AEvent,AMap), DOMEvent)
import           Replica.VDOM.Diff          (Diff, AttrDiff, diff, patch, diffAttrs, patchAttrs)
import           Replica.VDOM.Render        (renderHTML)

t :: T.Text -> T.Text
t = id

type Path = [Int]

fireWithAttrs :: Attrs -> T.Text -> DOMEvent -> IO ()
fireWithAttrs attrs evtName evtValue = case M.lookup evtName attrs of
  Just (AEvent attrEvent) -> attrEvent evtValue
  _ -> pure ()

fireEvent :: HTML -> Path -> T.Text -> DOMEvent -> IO ()
fireEvent _ []      = \_ _ -> pure ()
fireEvent ds (x:xs) = if x < length ds
  then fireEventOnNode (ds !! x) xs
  else \_ _ -> pure ()
  where
    fireEventOnNode (VNode _ attrs _) []        = fireWithAttrs attrs
    fireEventOnNode (VLeaf _ attrs) []          = fireWithAttrs attrs
    fireEventOnNode (VNode _ _ children) (p:ps) = if p < length children
      then fireEventOnNode (children !! p) ps
      else \_ _ -> pure ()
    fireEventOnNode _ _                         = \_ _ -> pure ()

clientDriver :: B.ByteString
clientDriver = $(FE.embedFile "./js/dist/client.js")

stagedIndex :: B.ByteString
stagedIndex = $(lift
    $ replace "<script src=\"dist/client.js\"></script>"
        ("<script language=\"javascript\">\n"
        <> $(FE.embedFile "./js/dist/client.js")
        <> "</script>"
        )
    $(FE.embedFile "./js/index.html")
  )

index :: B.ByteString -> HTML -> B.ByteString
index title header =
  replace "<!-- HEADER -->" (htmlToBS header)
  $ replace "$TITLE" title
  $ stagedIndex
  where
    htmlToBS = T.encodeUtf8 . TL.toStrict . TB.toLazyText . renderHTML
