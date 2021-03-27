{-# LANGUAGE OverloadedStrings   #-}

module Replica.VDOM.Diff where

import           Data.Aeson                 ((.=))
import qualified Data.Aeson                 as A
import qualified Data.Text                  as T
import qualified Data.Map                   as M
import qualified Data.Algorithm.Diff        as D
import           Replica.VDOM.Types         (HTML, VDOM(VNode,VLeaf,VText,VRawText), Attrs, Attr(AText,ABool,AEvent,AMap))

t :: T.Text -> T.Text
t = id

data Diff
  = Delete !Int
  | Insert !Int !VDOM
  | Diff !Int ![AttrDiff] ![Diff]
  | ReplaceText !Int !T.Text

instance A.ToJSON Diff where
  toJSON (Delete i) = A.object
    [ "type"  .= t "delete"
    , "index" .= i
    ]
  toJSON (Insert i v) = A.object
    [ "type"  .= t "insert"
    , "dom"   .= v
    , "index" .= i
    ]
  toJSON (Diff i ads ds) = A.object
    [ "type"  .= t "diff"
    , "diff"  .= ds
    , "adiff" .= ads
    , "index" .= i
    ]
  toJSON (ReplaceText i text) = A.object
    [ "type"  .= t "replace_text"
    , "index" .= i
    , "text"  .= text
    ]

data AttrDiff
  = DeleteKey !T.Text
  | InsertKey !T.Text !Attr
  | DiffKey   !T.Text ![KeyDiff]

instance A.ToJSON AttrDiff where
  toJSON (DeleteKey k) = A.object
    [ "type" .= t "delete"
    , "key" .= k
    ]
  toJSON (InsertKey k v) = A.object
    [ "type" .= t "insert"
    , "key" .= k
    , "value" .= v
    ]
  toJSON (DiffKey k ds) = A.object
    [ "type" .= t "diff"
    , "key" .= k
    , "diff" .= ds
    ]

data KeyDiff
  = Replace !Attr
  | DiffMap ![AttrDiff]

instance A.ToJSON KeyDiff where
  toJSON (Replace v) = A.object
    [ "type" .= t "replace"
    , "value" .= v
    ]
  toJSON (DiffMap ds) = A.object
    [ "type" .= t "diff"
    , "diff" .= ds
    ]

diff :: HTML -> HTML -> [Diff]
diff a b = concatMap (uncurry toDiff) (zip vdiffs is)
  where
    go :: Int -> [D.Diff a] -> [Int]
    go i (D.First _:ds) = i:go i ds
    go i (_:ds) = i:go (i + 1) ds
    go _ [] = []

    vdiffs = D.getDiffBy eqNode a b
    is     = go 0 vdiffs

    toDiff :: D.Diff VDOM -> Int -> [Diff]
    toDiff (D.First _) i  = [Delete i]
    toDiff (D.Second v) i = [Insert i v]
    toDiff (D.Both (VNode _ ca cNs c) v@(VNode _ da dNs d)) i
      | cNs /= dNs          = [Delete i, Insert i v]
      | null das && null ds = []
      | otherwise           = [Diff i (diffAttrs ca da) (diff c d)]
      where
        das = diffAttrs ca da
        ds  = diff c d
    toDiff (D.Both (VLeaf _ ca cNs) v@(VLeaf _ da dNs)) i
      | cNs /= dNs = [Delete i, Insert i v]
      | null das   = []
      | otherwise  = [Diff i (diffAttrs ca da) []]
      where
        das = diffAttrs ca da
    toDiff (D.Both (VText m) (VText n)) i
      | m == n    = []
      | otherwise = [ReplaceText i n]
    toDiff (D.Both (VRawText m) (VRawText n)) i
      | m == n    = []
      | otherwise = [ReplaceText i n]
    toDiff _ _ = []

    key attrs = M.lookup "key" attrs

    eqType (Just (AText m)) (Just (AText n))
      | m == n    = True
      | otherwise = False
    eqType Nothing Nothing = True
    eqType _ _ = False

    eqNode (VNode n na nNs _) (VNode m ma mNs _)
      | Just (AText k1) <- key na
      , Just (AText k2) <- key ma = k1 == k2
      | otherwise = n == m && nNs == mNs && M.lookup "type" na `eqType` M.lookup "type" ma
    eqNode (VLeaf n na nNs) (VLeaf m ma mNs)
      | Just (AText k1) <- key na
      , Just (AText k2) <- key ma = k1 == k2
      | otherwise = n == m && nNs == mNs && M.lookup "type" na `eqType` M.lookup "type" ma
    eqNode (VText _) (VText _) = True
    eqNode (VRawText _) (VRawText _) = True
    eqNode _ _ = False

diffAttrs :: Attrs -> Attrs -> [AttrDiff]
diffAttrs a b
  =  fmap DeleteKey (M.keys deleted)
  <> fmap (uncurry InsertKey) (M.assocs inserted)
  <> concatMap diffKey (M.assocs same)
  where
    deleted  = a `M.difference` b
    inserted = b `M.difference` a
    same     = M.intersectionWith (,) a b

    diffKey :: (T.Text, (Attr, Attr)) -> [AttrDiff]
    diffKey (k, (m, n))
      | null ds   = []
      | otherwise = [DiffKey k ds]
      where
        ds = diffVValue m n

    diffVValue :: Attr -> Attr -> [KeyDiff]
    diffVValue (AText m) vn@(AText n)
      | m == n = []
      | otherwise = [Replace vn]
    diffVValue (ABool m) vn@(ABool n)
      | m == n = []
      | otherwise = [Replace vn]
    diffVValue (AEvent m _) vn@(AEvent n _)
       | m == n = []
       | otherwise = [Replace vn]
    diffVValue (AMap m) (AMap n)
      | null das  = []
      | otherwise = [DiffMap $ diffAttrs m n]
      where
        das = diffAttrs m n
    diffVValue _ n                   = [Replace n]

patch :: [Diff] -> HTML -> HTML
patch [] a                  = a
patch (Delete i:rds) a      = patch rds $ take i a <> drop (i + 1) a
patch (Insert i v:rds) a    = patch rds $ take i a <> [v] <> drop i a
patch (Diff i ads ds:rds) a = patch rds $ take i a <> [v] <> drop (i + 1) a
  where
    v = case a !! i of
      VNode e as ns cs -> VNode e (patchAttrs ads as) ns (patch ds cs)
      VLeaf e as ns    -> VLeaf e (patchAttrs ads as) ns
      VText _          -> error "Can't node patch text"
      VRawText _       -> error "Can't node patch text"
patch (ReplaceText i n:rds) a = patch rds $ take i a <> [v] <> drop (i + 1) a
  where
    v = case a !! i of
      VText _     -> VText n
      VRawText _  -> VRawText n
      _           -> error "Can't text patch node"

patchAttrs :: [AttrDiff] -> Attrs -> Attrs
patchAttrs [] a                 = a
patchAttrs (DeleteKey k:ds) a   = patchAttrs ds $ M.delete k a
patchAttrs (InsertKey k v:ds) a = patchAttrs ds $ M.insert k v a
patchAttrs (DiffKey k vds:ds) a = patchAttrs ds $ M.adjust (patchVValue vds) k a
  where
    patchVValue [] v                      = v
    patchVValue (Replace m:vs) _          = patchVValue vs m
    patchVValue (DiffMap ads:vs) (AMap m) = patchVValue vs $ AMap (patchAttrs ads m)
    patchVValue (DiffMap _:_) _           = error "Can't patch map non-maps"
