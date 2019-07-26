{-# LANGUAGE OverloadedStrings   #-}

module Replica.VDOM.Types where

import           Data.Aeson                 ((.=))
import qualified Data.Aeson                 as A
import qualified Data.Text                  as T
import qualified Data.Map                   as M

t :: T.Text -> T.Text
t = id

type HTML = [VDOM]

data VDOM
  = VNode    !T.Text !Attrs ![VDOM]
  | VLeaf    !T.Text !Attrs
  | VText    !T.Text
  | VRawText !T.Text

instance A.ToJSON VDOM where
  toJSON (VText text) = A.object
    [ "type" .= t "text"
    , "text" .= text
    ]
  toJSON (VRawText text) = A.object
    [ "type" .= t "text"
    , "text" .= text
    ]
  toJSON (VLeaf element attrs) = A.object
    [ "type"    .= t "leaf"
    , "element" .= element
    , "attrs"   .= attrs
    ]
  toJSON (VNode element attrs children) = A.object
    [ "type"     .= t "node"
    , "element"  .= element
    , "attrs"    .= attrs
    , "children" .= children
    ]

type Attrs' a = M.Map T.Text (Attr' a)
type Attrs    = Attrs' (IO ())

data Attr' a
  = AText  !T.Text
  | ABool  !Bool
  | AEvent !(DOMEvent -> a)
  | AMap   !(Attrs' a)

type Attr = Attr' (IO ())

instance Semigroup (Attr' a) where
  AMap m <> AMap n = AMap (m <> n)
  _ <> m = m

instance A.ToJSON (Attr' a) where
  toJSON (AText v) = A.String v
  toJSON (ABool v)  = A.Bool v
  toJSON (AEvent _) = A.Null
  toJSON (AMap v)   = A.toJSON $ fmap A.toJSON v

newtype DOMEvent = DOMEvent { getDOMEvent :: A.Value }
