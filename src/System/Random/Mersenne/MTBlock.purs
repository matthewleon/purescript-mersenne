module System.Random.Mersenne.MTBlock (
  seedBlock
, nextBlock
, lookup
, n
) where

import Prelude
import Control.Monad.Eff (Eff, forE)
import Control.Monad.ST (ST, pureST)
import Data.Array ((!!), fromFoldable)
import Data.Array.ST (STArray, unsafeFreeze, thaw, peekSTArray, pokeSTArray)
import Data.Maybe (Maybe(..), fromJust)
import Data.Int.Bits ((.&.), (.|.), (.^.), shl, zshr)
import Data.List ((:))
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr)

import Partial.Unsafe (unsafePartial)

import System.Random.Mersenne.Types (MTBlock(..), Seed(..))

seedBlock :: Seed -> MTBlock
seedBlock (Seed i) = MTBlock seedArr
  where
    seedArr :: Array Int
    seedArr = fromFoldable $ firstEntry : unfoldr go (Tuple 1 firstEntry)

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
nextBlock (MTBlock arr) = MTBlock $ run do
  stArr <- thaw arr
  forE 0 n \index -> do
    thisEntry <- unsafePeekSTArray stArr index
    nextEntry <- unsafePeekSTArray stArr ((index + 1) `mod` n)
    furtherEntry <- unsafePeekSTArray stArr ((index + m) `mod` n)
    let y = (thisEntry .&. upperMask) .|. (nextEntry .&. lowerMask)
        new = furtherEntry .^. (y `zshr` 1) .^. mag (y .&. 0x1)
    void $ pokeSTArray stArr index new
  pure stArr
  where
    run :: forall a. (forall h. Eff (st :: ST h) (STArray h a)) -> Array a
    run act = pureST (act >>= unsafeFreeze)

    mag :: Int -> Int
    mag 0 = 0
    mag _ = -1727483681

    unsafePeekSTArray :: forall a h r.
                         STArray h a -> Int -> Eff (st :: ST h | r) a
    unsafePeekSTArray a i = unsafePartial (fromJust <$> peekSTArray a i)

lookup :: MTBlock -> Int -> Int
lookup (MTBlock arr) i = unsafePartial $ fromJust (arr !! (i `mod` n))

n :: Int
n = 624

m :: Int
m = 397

upperMask :: Int
upperMask = -2147483648 --0x80000000

lowerMask :: Int
lowerMask = 0x7fffffff
