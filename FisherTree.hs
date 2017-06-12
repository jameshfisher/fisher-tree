module FisherTrie where

import Data.Map (Map)
import qualified Data.Map as M
import Data.List (stripPrefix)

splitPrefix :: Eq a => [a] -> [a] -> ([a], [a], [a])
splitPrefix (x:xs) (y:ys) | x == y    = (x:p, xs', ys') where (p, xs', ys') = splitPrefix xs ys
splitPrefix    xs     ys              = ([], xs, ys)

type Height = Int  -- not required at runtime but useful for assertions

-- Represents a non-empty map!
-- To represent an empty map, use Maybe (or similar).
data FisherNode v
  = FisherZero v  -- maps empty string to v
  | FisherSucc Height [Char] (Either v (Map Char (FisherNode v)))
  deriving (Eq)

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

fisherFindNonEmpty :: String -> FisherNode v -> Maybe v
fisherFindNonEmpty "" (FisherZero v) = Just v
fisherFindNonEmpty s  (FisherSucc h s'     (Left v)) = if s == s' then Just v else Nothing
fisherFindNonEmpty s  (FisherSucc h prefix (Right m)) =
  case stripPrefix prefix s of
    Nothing -> Nothing
    Just (c:rest) ->  case M.lookup c m of
                        Nothing -> Nothing
                        Just f  -> fisherFindNonEmpty rest f

fisherFind :: String -> Maybe (FisherNode v) ->  Maybe v
fisherFind _ Nothing  = Nothing
fisherFind s (Just f) = fisherFindNonEmpty s f

fisherSingleton :: String -> v -> FisherNode v
fisherSingleton "" v = FisherZero v
fisherSingleton s v = FisherSucc (length s) s (Left v)

fisherInsertNonEmpty :: String -> v -> FisherNode v -> FisherNode v
fisherInsertNonEmpty "" v (FisherZero _) = FisherZero v
fisherInsertNonEmpty s  v (FisherSucc h prefix next) =
  case splitPrefix prefix s of
    (_      , [],        sRem  ) -> case next of
                                      Left _  -> FisherSucc h s (Left v)
                                      Right m -> FisherSucc h prefix (Right $ M.alter fn (head sRem) m) where
                                                  fn Nothing   = Just $ fisherSingleton (tail sRem) v
                                                  fn (Just ft) = Just $ fisherInsertNonEmpty (tail sRem) v ft
    (prefix', pc:prefCs, sc:sCs) -> FisherSucc h prefix' (Right $ M.fromList [
                                      (pc, FisherSucc (h - length prefix' - 1) prefCs next),
                                      (sc, FisherSucc (h - length prefix' - 1) sCs    (Left v))
                                    ])

assertions :: Bool
assertions = all id [
  splitPrefix "foo" "food" == ("foo", "", "d"),
  splitPrefix "foo" "bar" == ("", "foo", "bar"),
  isValidNonEmptyFisherNodeAtHeight 0 (FisherZero "bar"),
  isValidNonEmptyFisherNodeAtHeight 4 $ FisherSucc 4 "foo" $ Right $ M.fromList [('d', FisherZero "food"), ('l', FisherZero "fool"), ('t', FisherZero "foot")],
  isValidFisherNodeAtHeight 4 Nothing,
  isValidFisherNodeAtHeight 4 $ Just $ FisherSucc 4 "foo" $ Right $ M.fromList [('d', FisherZero "food"), ('l', FisherZero "fool"), ('t', FisherZero "foot")],
  toList (FisherZero "bar") == [("", "bar")],
  toList (FisherSucc 4 "foo" $ Right $ M.fromList [('d', FisherZero "food"), ('l', FisherZero "fool"), ('t', FisherZero "foot")]) == [("food", "food"), ("fool", "fool"), ("foot", "foot")],
  fisherFindNonEmpty "food" (FisherSucc 4 "foo" $ Right $ M.fromList [('d', FisherZero "food"), ('l', FisherZero "fool"), ('t', FisherZero "foot")]) == Just "food",
  fisherFindNonEmpty "foop" (FisherSucc 4 "foo" $ Right $ M.fromList [('d', FisherZero "food"), ('l', FisherZero "fool"), ('t', FisherZero "foot")]) == Nothing,
  fisherFind "food" Nothing == (Nothing :: Maybe String),
  fisherFind "food" (Just $ FisherSucc 4 "foo" $ Right $ M.fromList [('d', FisherZero "food"), ('l', FisherZero "fool"), ('t', FisherZero "foot")]) == Just "food",
  fisherInsertNonEmpty "fool" "fool" (FisherSucc 4 "foo" $ Right $ M.fromList [('d', FisherZero "food"), ('t', FisherZero "foot")]) == (FisherSucc 4 "foo" $ Right $ M.fromList [('d', FisherZero "food"), ('l', FisherZero "fool"), ('t', FisherZero "foot")])
  ]
