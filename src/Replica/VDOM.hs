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
import qualified Data.Map                   as M

import           Replica.VDOM.Types         (HTML, VDOM(VNode,VLeaf,VText,VRawText), Attrs, Attr(AText,ABool,AEvent,AMap), DOMEvent)
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

defaultIndex :: T.Text -> HTML -> HTML
defaultIndex title header =
  [ VLeaf "meta" (fl [("charset", AText "utf-8")])
  , VLeaf "!doctype" (fl [("html", ABool True)])
  , VNode "html" mempty
      [ VNode "head" mempty ([VNode "title" mempty [VText title]] <> header)
      , VNode "body" mempty
          [ VNode "script" (fl [("language", AText "javascript")])
              [ VRawText $ T.decodeUtf8 clientDriver ]
          ]
      ]
  ]
  where
    fl = M.fromList
