module FisherTrie where

import Data.Map (Map)
import qualified Data.Map as M
import Data.List (stripPrefix)
import System.Process

splitPrefix :: Eq a => [a] -> [a] -> ([a], [a], [a])
splitPrefix (x:xs) (y:ys) | x == y    = (x:p, xs', ys') where (p, xs', ys') = splitPrefix xs ys
splitPrefix    xs     ys              = ([], xs, ys)

type Height = Int  -- not required at runtime but useful for assertions

-- To represent an empty map, use Maybe (or similar).
data FisherNotEmpty v
  = FisherSucc Height [Char] (Either v (Map Char (FisherNotEmpty v)))
  deriving (Eq, Show)

type Fisher v = Maybe (FisherNotEmpty v)

-- Association lists are the simplest data structure to express a map.
-- Haskell libraries use association lists as an exchange format.
fisherNotEmptyToList :: FisherNotEmpty v -> [(String, v)]
fisherNotEmptyToList (FisherSucc _ prefix (Left v)) = [(prefix, v)]
fisherNotEmptyToList (FisherSucc _ prefix (Right m)) =
  concatMap (\(c, f) -> map (\(s, v) -> (prefix ++ c:s, v)) (fisherNotEmptyToList f)) $ M.toList m

fisherToList :: Fisher v -> [(String, v)]
fisherToList Nothing = []
fisherToList (Just f) = fisherNotEmptyToList f

fisherNotEmptyIsValidAtHeight :: Height -> FisherNotEmpty v -> Bool
fisherNotEmptyIsValidAtHeight h (FisherSucc h' prefix (Left _)) =
  h == h' && h == length prefix
fisherNotEmptyIsValidAtHeight h (FisherSucc h' prefix (Right m)) =
  h == h' &&
  0 < length prefix && length prefix < h &&
  1 < num_branches && num_branches <= 256 &&
  all (fisherNotEmptyIsValidAtHeight after_prefix_and_branches_height) (M.elems m)
  where
    after_prefix_height = h - length prefix
    after_prefix_and_branches_height = after_prefix_height - 1
    num_branches = M.size m

fisherIsValidAtHeight :: Height -> Fisher v -> Bool
fisherIsValidAtHeight h Nothing = True
fisherIsValidAtHeight h (Just f) = fisherNotEmptyIsValidAtHeight h f

fisherNotEmptyFind :: String -> FisherNotEmpty v -> Maybe v
fisherNotEmptyFind s  (FisherSucc h s'     (Left v)) = if s == s' then Just v else Nothing
fisherNotEmptyFind s  (FisherSucc h prefix (Right m)) =
  case stripPrefix prefix s of
    Nothing -> Nothing
    Just (c:rest) ->  case M.lookup c m of
                        Nothing -> Nothing
                        Just f  -> fisherNotEmptyFind rest f

fisherFind :: String -> Fisher v -> Maybe v
fisherFind _ Nothing  = Nothing
fisherFind s (Just f) = fisherNotEmptyFind s f

fisherSingleton :: String -> v -> FisherNotEmpty v
fisherSingleton s v = FisherSucc (length s) s (Left v)

fisherNotEmptyInsert :: String -> v -> FisherNotEmpty v -> FisherNotEmpty v
fisherNotEmptyInsert s  v (FisherSucc h prefix next) =
  case splitPrefix prefix s of
    (_      , [],        sRem  ) -> case next of
                                      Left _  -> FisherSucc h s (Left v)
                                      Right m -> FisherSucc h prefix (Right $ M.alter fn (head sRem) m) where
                                                  fn Nothing   = Just $ fisherSingleton (tail sRem) v
                                                  fn (Just ft) = Just $ fisherNotEmptyInsert (tail sRem) v ft
    (prefix', pc:prefCs, sc:sCs) -> FisherSucc h prefix' (Right $ M.fromList [
                                      (pc, FisherSucc (h - length prefix' - 1) prefCs next),
                                      (sc, FisherSucc (h - length prefix' - 1) sCs    (Left v))
                                    ])

fisherInsert :: String -> v -> Fisher v -> Fisher v
fisherInsert s v Nothing  = Just $ fisherSingleton s v
fisherInsert s v (Just f) = Just $ fisherNotEmptyInsert s v f

fisherNotEmptyDelete :: String -> FisherNotEmpty v -> Fisher v
fisherNotEmptyDelete s t@(FisherSucc h s' (Left v)) =
  if s == s' then Nothing else Just t
fisherNotEmptyDelete s t@(FisherSucc h prefix (Right m)) = case splitPrefix prefix s of
  (_, [], (c:cs)) -> case M.toList newM of
                      [(c', FisherSucc h' prefix' next)] -> Just $ FisherSucc h (prefix ++ [c'] ++ prefix') next
                      _                                  -> Just $ FisherSucc h prefix (Right newM)
                     where
                      newM = M.alter fn c m
                      fn Nothing = Nothing
                      fn (Just f) = fisherNotEmptyDelete cs f
  _ -> Just t

fisherDelete :: String -> Fisher v -> Fisher v
fisherDelete s Nothing  = Nothing
fisherDelete s (Just f) = fisherNotEmptyDelete s f

assertions :: Bool
assertions = all id [
  splitPrefix "foo" "food" == ("foo", "", "d"),
  splitPrefix "foo" "bar" == ("", "foo", "bar"),
  fisherNotEmptyIsValidAtHeight 0 (FisherSucc 0 "" (Left "bar")),
  fisherNotEmptyIsValidAtHeight 4 $ FisherSucc 4 "foo" $ Right $ M.fromList [('d', FisherSucc 0 "" (Left "food")), ('l', FisherSucc 0 "" (Left "fool")), ('t', FisherSucc 0 "" (Left "foot"))],
  fisherIsValidAtHeight 4 Nothing,
  fisherIsValidAtHeight 4 $ Just $ FisherSucc 4 "foo" $ Right $ M.fromList [('d', FisherSucc 0 "" (Left "food")), ('l', FisherSucc 0 "" (Left "fool")), ('t', FisherSucc 0 "" (Left "foot"))],
  fisherNotEmptyToList (FisherSucc 0 "" (Left "bar")) == [("", "bar")],
  fisherNotEmptyToList (FisherSucc 4 "foo" $ Right $ M.fromList [('d', FisherSucc 0 "" (Left "food")), ('l', FisherSucc 0 "" (Left "fool")), ('t', FisherSucc 0 "" (Left "foot"))]) == [("food", "food"), ("fool", "fool"), ("foot", "foot")],
  fisherNotEmptyFind "food" (FisherSucc 4 "foo" $ Right $ M.fromList [('d', FisherSucc 0 "" (Left "food")), ('l', FisherSucc 0 "" (Left "fool")), ('t', FisherSucc 0 "" (Left "foot"))]) == Just "food",
  fisherNotEmptyFind "foop" (FisherSucc 4 "foo" $ Right $ M.fromList [('d', FisherSucc 0 "" (Left "food")), ('l', FisherSucc 0 "" (Left "fool")), ('t', FisherSucc 0 "" (Left "foot"))]) == Nothing,
  fisherFind "food" Nothing == (Nothing :: Maybe String),
  fisherFind "food" (Just $ FisherSucc 4 "foo" $ Right $ M.fromList [('d', FisherSucc 0 "" (Left "food")), ('l', FisherSucc 0 "" (Left "fool")), ('t', FisherSucc 0 "" (Left "foot"))]) == Just "food",
  fisherNotEmptyInsert "fool" "fool" (FisherSucc 4 "foo" $ Right $ M.fromList [('d', FisherSucc 0 "" (Left "food")), ('t', FisherSucc 0 "" (Left "foot"))]) == (FisherSucc 4 "foo" $ Right $ M.fromList [('d', FisherSucc 0 "" (Left "food")), ('l', FisherSucc 0 "" (Left "fool")), ('t', FisherSucc 0 "" (Left "foot"))]),
  fisherInsert "food" 5 Nothing == (Just $ FisherSucc 4 "food" (Left 5)),
  fisherNotEmptyDelete "bar" (FisherSucc 3 "" (Right (M.fromList [('b',FisherSucc 2 "ar" (Left 11)),('f',FisherSucc 2 "oo" (Left 5))]))) == Just (FisherSucc 3 "foo" (Left 5))
  ]

data Op
  = OpInsert String Int
  | OpDelete String

-- TODO
randOps :: IO [Op]
randOps = return [OpInsert "foo" 5, OpInsert "bar" 10, OpInsert "bar" 11, OpDelete "foo"]

fisherRunOp :: Op -> Fisher Int -> Fisher Int
fisherRunOp (OpInsert k v) f = fisherInsert k v f
fisherRunOp (OpDelete k) f = fisherDelete k f

mapRunOp :: Op -> Map String Int -> Map String Int
mapRunOp (OpInsert k v) = M.insert k v
mapRunOp (OpDelete k) = M.delete k

-- stupidly, System.Random is not part of the standard library.
-- so we have to create our own random. Returns in [0,1).
randomFloat :: IO Float
randomFloat = do
  str <- System.Process.readProcess "node" ["-e", "console.log(Math.random())"] ""
  return $ read str

runTest :: IO Bool
runTest = do
  ops <- randOps
  let tree = foldl (flip fisherRunOp) Nothing ops
  let m    = foldl (flip mapRunOp) M.empty ops
  let l1 = fisherToList tree
  let l2 = M.toList m
  print tree
  print l1
  print l2
  return $ l1 == l2
