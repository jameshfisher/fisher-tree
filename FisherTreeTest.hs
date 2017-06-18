module FisherTreeTest where

import Data.Map (Map)
import qualified Data.Map as M
import System.Process
import Data.Char (chr, ord)
import Control.Monad (replicateM)
import GHC.Word (Word8)
import qualified Data.ByteString as B

import FisherTree (Fisher, fisherInsert, fisherDelete, fisherToList, fisherIsValidAtHeight)

data Op
  = OpInsert [Word8] [Word8]
  | OpDelete [Word8]
  deriving (Show)

fisherRunOp :: Op -> Fisher [Word8] -> Fisher [Word8]
fisherRunOp (OpInsert k v) = fisherInsert k v
fisherRunOp (OpDelete k) = fisherDelete k

mapRunOp :: Op -> Map [Word8] [Word8] -> Map [Word8] [Word8]
mapRunOp (OpInsert k v) = M.insert k v
mapRunOp (OpDelete k) = M.delete k

-- stupidly, System.Random is not part of the standard library.
-- so we have to create our own random. Returns in [0,1).
-- it is EXTREMELY slow!
randomFloat :: IO Float
randomFloat = do
  str <- System.Process.readProcess "node" ["-e", "console.log(Math.random())"] ""
  return $ read str

randomWord8 :: IO Word8
randomWord8 = do
  f <- randomFloat
  return $ floor (f*5.0)  -- restrict byte range for testing

randomBytes :: IO [Word8]
randomBytes = replicateM 3 randomWord8

randomOp :: IO Op
randomOp = do
  f <- randomFloat
  k <- randomBytes
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
  if fisherIsValidAtHeight 3 tree
    then return $ l1 == l2
    else error "Invalid tree"
