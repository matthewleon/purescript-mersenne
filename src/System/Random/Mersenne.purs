module System.Random.Mersenne (seed, int32) where

import Data.Tuple (Tuple)

import System.Random.Mersenne.Types (MTState, Seed(..))
import System.Random.Mersenne.State (init, next)

seed :: Int -> MTState
seed i = init (Seed i)

int32 :: MTState -> Tuple Int MTState
int32 = next
