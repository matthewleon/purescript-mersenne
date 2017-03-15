module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Now (NOW, now)
import Data.DateTime.Instant (unInstant)
import Data.List (length, reverse)
import Data.List.Types (List(..), (:))
import Data.Tuple (Tuple(..))

import System.Random.Mersenne (seed, int32)
import System.Random.Mersenne.Types (MTState)

main :: Eff (now :: NOW, console :: CONSOLE) Unit
main = do
  start <- now
  let ints = randomInts 100 1000000
  generated <- now
  log $ "generated list of " <> show (length ints) <> " random ints."
  logShow (unInstant generated - unInstant start)
  where
    randomInts :: Int -> Int -> List Int
    randomInts s length = reverse $ go (seed s) Nil 0
      where
        go :: MTState -> List Int -> Int -> List Int
        -- tail recursive, builds backward
        go state accum index
          | (index < length) =
            case int32 state
              of (Tuple i state') -> go state' (i : accum) (index + 1)
          | otherwise = accum
