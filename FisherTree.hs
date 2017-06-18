module FisherTrie where

import Data.Map (Map)
import qualified Data.Map as M
import Data.List (stripPrefix)
import System.Process
import Data.Char (chr, ord)
import Control.Monad (replicateM)

splitPrefix :: Eq a => [a] -> [a] -> ([a], [a], [a])
splitPrefix (x:xs) (y:ys) | x == y    = (x:p, xs', ys') where (p, xs', ys') = splitPrefix xs ys
splitPrefix    xs     ys              = ([], xs, ys)

type Height = Int  -- not required at runtime but useful for assertions

-- To represent an empty map, use Maybe (or similar).
data FisherNotEmpty v
  = FisherNotEmpty Height [Char] (Either v (Map Char (FisherNotEmpty v)))
  deriving (Eq, Show)

type Fisher v = Maybe (FisherNotEmpty v)

-- Association lists are the simplest data structure to express a map.
-- Haskell libraries use association lists as an exchange format.
fisherNotEmptyToList :: FisherNotEmpty v -> [(String, v)]
fisherNotEmptyToList (FisherNotEmpty _ prefix (Left v)) = [(prefix, v)]
fisherNotEmptyToList (FisherNotEmpty _ prefix (Right m)) =
  concatMap (\(c, f) -> map (\(s, v) -> (prefix ++ c:s, v)) (fisherNotEmptyToList f)) $ M.toList m

fisherToList :: Fisher v -> [(String, v)]
fisherToList Nothing = []
fisherToList (Just f) = fisherNotEmptyToList f

fisherNotEmptyIsValidAtHeight :: Height -> FisherNotEmpty v -> Bool
fisherNotEmptyIsValidAtHeight h (FisherNotEmpty h' prefix (Left _)) =
  h == h' && h == length prefix
fisherNotEmptyIsValidAtHeight h (FisherNotEmpty h' prefix (Right m)) =
  h == h' &&
  0 < length prefix && length prefix < h &&
  1 < num_branches && num_branches <= 256 &&
  all (fisherNotEmptyIsValidAtHeight after_prefix_and_branches_height) (M.elems m)
  where
    after_prefix_height = h - length prefix
    after_prefix_and_branches_height = after_prefix_height - 1
    num_branches = M.size m

fisherIsValidAtHeight :: Height -> Fisher v -> Bool
fisherIsValidAtHeight _ Nothing = True
fisherIsValidAtHeight h (Just f) = fisherNotEmptyIsValidAtHeight h f

fisherNotEmptyFind :: String -> FisherNotEmpty v -> Maybe v
fisherNotEmptyFind s (FisherNotEmpty _ prefix next) =
  case stripPrefix prefix s of
    Nothing -> Nothing
    Just rest -> case next of
                  Left v -> Just v  -- here we know rest == ""
                  Right m -> case M.lookup (head rest) m of
                              Nothing -> Nothing
                              Just f  -> fisherNotEmptyFind (tail rest) f

fisherFind :: String -> Fisher v -> Maybe v
fisherFind _ Nothing  = Nothing
fisherFind s (Just f) = fisherNotEmptyFind s f

fisherSingleton :: String -> v -> FisherNotEmpty v
fisherSingleton s v = FisherNotEmpty (length s) s (Left v)

fisherNotEmptyInsert :: String -> v -> FisherNotEmpty v -> FisherNotEmpty v
fisherNotEmptyInsert s  v (FisherNotEmpty h prefix next) =
  case splitPrefix prefix s of
    (_      , [],        sRem  ) -> case next of
                                      Left _  -> FisherNotEmpty h s (Left v)
                                      Right m -> FisherNotEmpty h prefix (Right $ M.alter fn (head sRem) m) where
                                                  fn Nothing   = Just $ fisherSingleton (tail sRem) v
                                                  fn (Just ft) = Just $ fisherNotEmptyInsert (tail sRem) v ft
    (prefix', pc:prefCs, sc:sCs) -> FisherNotEmpty h prefix' (Right $ M.fromList [
                                      (pc, FisherNotEmpty (h - length prefix' - 1) prefCs next),
                                      (sc, FisherNotEmpty (h - length prefix' - 1) sCs    (Left v))
                                    ])
    _ -> error "inserted string must be different length to height of tree"

fisherInsert :: String -> v -> Fisher v -> Fisher v
fisherInsert s v Nothing  = Just $ fisherSingleton s v
fisherInsert s v (Just f) = Just $ fisherNotEmptyInsert s v f

fisherNotEmptyDelete :: String -> FisherNotEmpty v -> Fisher v
fisherNotEmptyDelete s t@(FisherNotEmpty _ s' (Left _)) =
  if s == s' then Nothing else Just t
fisherNotEmptyDelete s t@(FisherNotEmpty h prefix (Right m)) = case splitPrefix prefix s of
  (_, [], (c:cs)) -> case M.toList newM of
                      [(c', FisherNotEmpty _ prefix' next)] -> Just $ FisherNotEmpty h (prefix ++ [c'] ++ prefix') next
                      _                                  -> Just $ FisherNotEmpty h prefix (Right newM)
                     where
                      newM = M.alter fn c m where
                        fn Nothing = Nothing
                        fn (Just f) = fisherNotEmptyDelete cs f
  _ -> Just t

fisherDelete :: String -> Fisher v -> Fisher v
fisherDelete _ Nothing  = Nothing
fisherDelete s (Just f) = fisherNotEmptyDelete s f

assertions :: Bool
assertions = all id [
  splitPrefix "foo" "food" == ("foo", "", "d"),
  splitPrefix "foo" "bar" == ("", "foo", "bar"),
  fisherNotEmptyIsValidAtHeight 0 (FisherNotEmpty 0 "" (Left "bar")),
  fisherNotEmptyIsValidAtHeight 4 $ FisherNotEmpty 4 "foo" $ Right $ M.fromList [('d', FisherNotEmpty 0 "" (Left "food")), ('l', FisherNotEmpty 0 "" (Left "fool")), ('t', FisherNotEmpty 0 "" (Left "foot"))],
  fisherIsValidAtHeight 4 Nothing,
  fisherIsValidAtHeight 4 $ Just $ FisherNotEmpty 4 "foo" $ Right $ M.fromList [('d', FisherNotEmpty 0 "" (Left "food")), ('l', FisherNotEmpty 0 "" (Left "fool")), ('t', FisherNotEmpty 0 "" (Left "foot"))],
  fisherNotEmptyToList (FisherNotEmpty 0 "" (Left "bar")) == [("", "bar")],
  fisherNotEmptyToList (FisherNotEmpty 4 "foo" $ Right $ M.fromList [('d', FisherNotEmpty 0 "" (Left "food")), ('l', FisherNotEmpty 0 "" (Left "fool")), ('t', FisherNotEmpty 0 "" (Left "foot"))]) == [("food", "food"), ("fool", "fool"), ("foot", "foot")],
  fisherNotEmptyFind "food" (FisherNotEmpty 4 "foo" $ Right $ M.fromList [('d', FisherNotEmpty 0 "" (Left "food")), ('l', FisherNotEmpty 0 "" (Left "fool")), ('t', FisherNotEmpty 0 "" (Left "foot"))]) == Just "food",
  fisherNotEmptyFind "foop" (FisherNotEmpty 4 "foo" $ Right $ M.fromList [('d', FisherNotEmpty 0 "" (Left "food")), ('l', FisherNotEmpty 0 "" (Left "fool")), ('t', FisherNotEmpty 0 "" (Left "foot"))]) == Nothing,
  fisherFind "food" Nothing == (Nothing :: Maybe String),
  fisherFind "food" (Just $ FisherNotEmpty 4 "foo" $ Right $ M.fromList [('d', FisherNotEmpty 0 "" (Left "food")), ('l', FisherNotEmpty 0 "" (Left "fool")), ('t', FisherNotEmpty 0 "" (Left "foot"))]) == Just "food",
  fisherNotEmptyInsert "fool" "fool" (FisherNotEmpty 4 "foo" $ Right $ M.fromList [('d', FisherNotEmpty 0 "" (Left "food")), ('t', FisherNotEmpty 0 "" (Left "foot"))]) == (FisherNotEmpty 4 "foo" $ Right $ M.fromList [('d', FisherNotEmpty 0 "" (Left "food")), ('l', FisherNotEmpty 0 "" (Left "fool")), ('t', FisherNotEmpty 0 "" (Left "foot"))]),
  fisherInsert "food" (5::Integer) Nothing == (Just $ FisherNotEmpty 4 "food" (Left 5)),
  fisherNotEmptyDelete "bar" (FisherNotEmpty 3 "" (Right (M.fromList [('b',FisherNotEmpty 2 "ar" (Left (11 :: Integer))),('f',FisherNotEmpty 2 "oo" (Left 5))]))) == Just (FisherNotEmpty 3 "foo" (Left 5))
  ]

data Op
  = OpInsert String String
  | OpDelete String
  deriving (Show)

fisherRunOp :: Op -> Fisher String -> Fisher String
fisherRunOp (OpInsert k v) = fisherInsert k v
fisherRunOp (OpDelete k) = fisherDelete k

mapRunOp :: Op -> Map String String -> Map String String
mapRunOp (OpInsert k v) = M.insert k v
mapRunOp (OpDelete k) = M.delete k

-- stupidly, System.Random is not part of the standard library.
-- so we have to create our own random. Returns in [0,1).
-- it is EXTREMELY slow!
randomFloat :: IO Float
randomFloat = do
  str <- System.Process.readProcess "node" ["-e", "console.log(Math.random())"] ""
  return $ read str

randomChar :: IO Char
randomChar = do
  f <- randomFloat
  return $ chr $ ord 'a' + floor (f*5.0)

randomString :: IO String
randomString = replicateM 3 randomChar

randomOp :: IO Op
randomOp = do
  f <- randomFloat
  k <- randomString
  if f < 0.5
    then return $ OpInsert k k
    else return $ OpDelete k

-- TODO
randomOps :: IO [Op]
randomOps = replicateM 10 randomOp

runTest :: IO Bool
runTest = do
  ops <- randomOps
  let tree = foldl (flip fisherRunOp) Nothing ops
  let m    = foldl (flip mapRunOp) M.empty ops
  let l1 = fisherToList tree
  let l2 = M.toList m
  return $ l1 == l2
