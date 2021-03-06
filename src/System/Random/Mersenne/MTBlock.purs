module System.Random.Mersenne.MTBlock (
  seedBlock
, nextBlock
, lookup
, n
) where

import Prelude
import Control.Monad.Eff (Eff, forE)
import Control.Monad.ST (ST, pureST)
import Data.Array ((:), unsafeIndex)
import Data.Array.ST (STArray, unsafeFreeze, thaw)
import Data.Maybe (Maybe(..))
import Data.Int.Bits ((.&.), (.|.), (.^.), shl, zshr)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr)

import Partial.Unsafe (unsafePartial)

import System.Random.Mersenne.Types (MTBlock(..), Seed(..))

seedBlock :: Seed -> MTBlock
seedBlock (Seed i) = MTBlock seedArr
  where
    seedArr :: Array Int
    --TODO: this is probably slow
    seedArr = firstEntry : unfoldr go (Tuple 1 firstEntry)

    firstEntry :: Int
    firstEntry = i `zshr` 0

    go :: Tuple Int Int -> Maybe (Tuple Int (Tuple Int Int))
    go (Tuple index previousEntry)
      | index < n = Just (iter index previousEntry)
      | otherwise = Nothing

    iter :: Int -> Int -> Tuple Int (Tuple Int Int)
    iter index previousEntry =
      let s = previousEntry .^. (previousEntry `zshr` 30)
          thisEntry =
            ((((s .&. -65536) `zshr` 16) * 1812433253) `shl` 16)
              + ((s .&. 65535) * 1812433253)
              + index
      in Tuple thisEntry (Tuple (index + 1) thisEntry)

nextBlock :: MTBlock -> MTBlock
nextBlock (MTBlock arr) = MTBlock $ pureST mkBlock
  where
    mkBlock :: forall h. Eff (st :: ST h) (Array Int)
    mkBlock = do
      stArr <- thaw arr
      let unsafePeek = unsafePeekSTArray stArr
      forE 0 n \index -> do
        thisEntry <- unsafePeek index
        nextEntry <- unsafePeek ((index + 1) `mod` n)
        furtherEntry <- unsafePeek ((index + m) `mod` n)
        let y = (thisEntry .&. upperMask) .|. (nextEntry .&. lowerMask)
            new = furtherEntry .^. (y `zshr` 1) .^.  ((y .&. 0x1) * -1727483681)
        unsafePokeSTArray stArr index new
      unsafeFreeze stArr

lookup :: MTBlock -> Int -> Int
lookup (MTBlock arr) i = unsafePartial $ arr `unsafeIndex` (i `mod` n)

n :: Int
n = 624

m :: Int
m = 397

upperMask :: Int
upperMask = -2147483648 --0x80000000

lowerMask :: Int
lowerMask = 0x7fffffff

foreign import unsafePeekSTArray
  :: forall a h r . STArray h a -> Int -> Eff (st :: ST h | r) a

foreign import unsafePokeSTArray
  :: forall a h r . STArray h a -> Int -> a -> Eff (st :: ST h | r) Unit
