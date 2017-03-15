module System.Random.Mersenne.State (init, next) where

import Prelude
import Data.Int.Bits ((.&.), (.^.), shl, zshr)
import Data.Tuple (Tuple(..))

import System.Random.Mersenne.Types (MTState(..), Seed)
import System.Random.Mersenne.MTBlock (seedBlock, lookup, nextBlock, n)

init :: Seed -> MTState
init s = MTState (seedBlock s) 0

next :: MTState -> Tuple Int MTState
next (MTState blk i) = Tuple (temper $ lookup blk' i) (MTState blk' i')
  where
    blk' = if i == 0 then nextBlock blk else blk
    i' = (i + 1) `mod` blockSize
    temper y =
      let y'   = y    .^. (y `zshr` 11)
          y''  = y'   .^. ((y' `shl` 7) .&. -1658038656)
          y''' = y''  .^. ((y'' `shl` 15) .&. -272236544)
      in         y''' .^. (y''' `zshr` 18)
    blockSize = n
