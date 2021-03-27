{-# LANGUAGE OverloadedStrings   #-}

module Replica.VDOM.Render where

import qualified Data.Text                  as T
import qualified Data.Text.Lazy.Builder     as TB
import qualified Data.Map                   as M

import           Replica.VDOM.Types         (HTML, VDOM(VNode,VLeaf,VText,VRawText), Attrs, Attr(AText,ABool,AEvent,AMap))

renderHTML :: HTML -> TB.Builder
renderHTML = mconcat . map renderVDOM

renderVDOM :: VDOM -> TB.Builder
renderVDOM vdom = case vdom of
  VNode name attrs _mNamespace children -> mconcat
    [ tag $ TB.fromText name <> renderAttrs attrs
    , mconcat $ map renderVDOM children
    , tag $ sl <> TB.fromText name
    ]
  VLeaf name attrs _mNamespace -> tag $ TB.fromText name <> renderAttrs attrs
  VText txt                    -> renderEscapedString txt
  VRawText txt                 -> TB.fromText txt
  where
    tag a = TB.singleton '<' <> a <> TB.singleton '>'
    sl = TB.singleton '/'

renderAttrs :: Attrs -> TB.Builder
renderAttrs = foldMap (TB.singleton ' ' <>) . _renderAttrs
  where
    dq = TB.singleton '"'
    eq = TB.singleton '='

    _renderAttrs :: Attrs -> [TB.Builder]
    _renderAttrs = foldMap (uncurry _renderAttr) . M.toList

    _renderAttr :: T.Text -> Attr -> [TB.Builder]
    _renderAttr name value = case value of
      AText txt   -> [TB.fromText name <> eq <> dq <> renderEscapedString txt <> dq]
      ABool True  -> [TB.fromText name]
      ABool False -> []
      AEvent _ _  -> []
      AMap attrs  -> _renderAttrs attrs

renderEscapedString :: T.Text -> TB.Builder
renderEscapedString = TB.fromText . T.concatMap escape
  where
    escape :: Char -> T.Text
    escape '<'  = "&lt;"
    escape '>'  = "&gt;"
    escape '&'  = "&amp;"
    escape '"'  = "&quot;"
    escape '\'' = "&#39;"
    escape c    = T.singleton c
