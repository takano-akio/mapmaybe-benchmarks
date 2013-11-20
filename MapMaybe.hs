module MapMaybe (mapMaybe) where

import GHC.Exts (build)

-- definition copied from Data.Maybe
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x:xs) = let
  rs = mapMaybe f xs
  in case f x of
    Nothing -> rs
    Just r -> r : rs

{-# RULES
"mapMaybe"     [~1] forall f xs. mapMaybe f xs
                    = build (\c n -> foldr (mapMaybeFB c f) n xs)
"mapMaybeList" [1]  forall f. foldr (mapMaybeFB (:) f) [] = mapMaybe f
  #-}

{-# NOINLINE [0] mapMaybeFB #-}
mapMaybeFB :: (b -> r -> r) -> (a -> Maybe b) -> a -> r -> r
mapMaybeFB cons f x next = case f x of
  Nothing -> next
  Just r -> cons r next
