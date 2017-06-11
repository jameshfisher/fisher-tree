module FisherTrie where

import Data.Map (Map)
import qualified Data.Map as M

type Height = Int  -- not required at runtime but useful for assertions

-- Represents a non-empty map!
-- To represent an empty map, use Maybe (or similar).
data FisherNode v
  = FisherZero v  -- maps empty string to v
  | FisherSucc Height [Char] (Either v (Map Char (FisherNode v)))

-- Association lists are the simplest data structure to express a map.
-- Haskell libraries use association lists as an exchange format.
toList :: FisherNode v -> [(String, v)]
toList (FisherZero v) = [("", v)]
toList (FisherSucc _ prefix (Left v)) = [(prefix, v)]
toList (FisherSucc _ prefix (Right m)) =
  concatMap (\(c, f) -> map (\(s, v) -> (prefix ++ c:s, v)) (toList f)) $ M.toList m

isValidNonEmptyFisherNodeAtHeight :: Height -> FisherNode v -> Bool
isValidNonEmptyFisherNodeAtHeight 0 (FisherZero _) = True
isValidNonEmptyFisherNodeAtHeight h (FisherSucc h' prefix (Left _)) =
  h == h' && h == length prefix
isValidNonEmptyFisherNodeAtHeight h (FisherSucc h' prefix (Right m)) =
  h == h' &&
  0 < length prefix && length prefix < h &&
  1 < num_branches && num_branches <= 256 &&
  all (isValidNonEmptyFisherNodeAtHeight after_prefix_and_branches_height) (M.elems m)
  where
    after_prefix_height = h - length prefix
    after_prefix_and_branches_height = after_prefix_height - 1
    num_branches = M.size m
isValidNonEmptyFisherNodeAtHeight h f = False

isValidFisherNodeAtHeight :: Height -> Maybe (FisherNode v) -> Bool
isValidFisherNodeAtHeight h Nothing = True
isValidFisherNodeAtHeight h (Just f) = isValidNonEmptyFisherNodeAtHeight h f

assertions :: Bool
assertions = all id [
  isValidNonEmptyFisherNodeAtHeight 0 (FisherZero "bar"),
  isValidNonEmptyFisherNodeAtHeight 4 $ FisherSucc 4 "foo" $ Right $ M.fromList [('d', FisherZero "food"), ('l', FisherZero "fool"), ('t', FisherZero "foot")],
  isValidFisherNodeAtHeight 4 Nothing,
  isValidFisherNodeAtHeight 4 $ Just $ FisherSucc 4 "foo" $ Right $ M.fromList [('d', FisherZero "food"), ('l', FisherZero "fool"), ('t', FisherZero "foot")],
  toList (FisherZero "bar") == [("", "bar")],
  toList (FisherSucc 4 "foo" $ Right $ M.fromList [('d', FisherZero "food"), ('l', FisherZero "fool"), ('t', FisherZero "foot")]) == [("food", "food"), ("fool", "fool"), ("foot", "foot")]
  ]