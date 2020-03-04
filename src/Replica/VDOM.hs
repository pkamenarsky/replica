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

import           Replica.VDOM.Types         (HTML, VDOM(VNode,VLeaf,VText,VRawText), Attrs, Attr(AText,ABool,AEvent,AMap), DOMEvent, Namespace(Namespace, getNamespace))
import           Replica.VDOM.Diff          (Diff, AttrDiff, diff, patch, diffAttrs, patchAttrs)
import           Replica.VDOM.Render        (renderHTML)

t :: T.Text -> T.Text
t = id

type Path = [Int]

fireWithAttrs :: Attrs -> T.Text -> DOMEvent -> Maybe (IO ())
fireWithAttrs attrs evtName evtValue = case M.lookup evtName attrs of
  Just (AEvent attrEvent) -> Just (attrEvent evtValue)
  _ -> Nothing

fireEvent
  :: HTML
  -> Path
  -> T.Text -- ^ Event name
  -> DOMEvent
  -> Maybe (IO ())
fireEvent _ []      = \_ _ -> Nothing
fireEvent ds (x:xs) = if x < length ds
  then fireEventOnNode (ds !! x) xs
  else \_ _ -> Nothing
  where
    fireEventOnNode (VNode _ attrs _ns _) []        = fireWithAttrs attrs
    fireEventOnNode (VLeaf _ attrs _ns) []          = fireWithAttrs attrs
    fireEventOnNode (VNode _ _ _ns children) (p:ps) = if p < length children
      then fireEventOnNode (children !! p) ps
      else \_ _ -> Nothing
    fireEventOnNode _ _                         = \_ _ -> Nothing

clientDriver :: B.ByteString
clientDriver = $(FE.embedFile "./js/dist/client.js")

defaultIndex :: T.Text -> HTML -> HTML
defaultIndex title header =
  [ VLeaf "!doctype" (fl [("html", ABool True)]) Nothing
  , VNode "html" mempty Nothing
      [ VNode "head" mempty Nothing $
          [ VLeaf "meta" (fl [("charset", AText "utf-8")]) Nothing
          , VNode "title" mempty Nothing [VText title]
          ]
          <> header
      , VNode "body" mempty Nothing
          [ VNode "script" (fl [("language", AText "javascript")]) Nothing
              [ VRawText $ T.decodeUtf8 clientDriver ]
          ]
      ]
  ]
  where
    fl = M.fromList
