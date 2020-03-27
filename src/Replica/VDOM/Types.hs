{-# LANGUAGE OverloadedStrings   #-}

module Replica.VDOM.Types where

import           Data.Aeson                 ((.=))
import qualified Data.Aeson                 as A
import qualified Data.Text                  as T
import qualified Data.Map                   as M

t :: T.Text -> T.Text
t = id

type HTML event = [VDOM event]

newtype Namespace = Namespace { getNamespace :: T.Text } deriving (Eq, Ord, Show)

data VDOM event
  = VNode    !T.Text !(Attrs event) !(Maybe Namespace) ![VDOM event]
  | VLeaf    !T.Text !(Attrs event) !(Maybe Namespace)
  | VText    !T.Text
  | VRawText !T.Text

instance A.ToJSON (VDOM event) where
  toJSON (VText text) = A.object
    [ "type" .= t "text"
    , "text" .= text
    ]
  toJSON (VRawText text) = A.object
    [ "type" .= t "text"
    , "text" .= text
    ]
  toJSON (VLeaf element attrs mNamespace) = A.object $
    [ "type"      .= t "leaf"
    , "element"   .= element
    , "attrs"     .= attrs
    , "namespace" .= fmap getNamespace mNamespace
    ]
  toJSON (VNode element attrs mNamespace children) = A.object $
    [ "type"      .= t "node"
    , "element"   .= element
    , "attrs"     .= attrs
    , "children"  .= children
    , "namespace" .= fmap getNamespace mNamespace
    ]

type Attrs event = M.Map T.Text (Attr event)

data Attr event
  = AText  !T.Text
  | ABool  !Bool
  | AEvent !event
  | AMap   !(Attrs event)

instance A.ToJSON (Attr event) where
  toJSON (AText v) = A.String v
  toJSON (ABool v)  = A.Bool v
  toJSON (AEvent _) = A.Null
  toJSON (AMap v)   = A.toJSON $ fmap A.toJSON v

newtype DOMEvent = DOMEvent { getDOMEvent :: A.Value }
