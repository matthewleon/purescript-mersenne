module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Console.BrowserSpecific.Timer (time, timeEnd)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array (fromFoldable)
import Data.List (length, reverse)
import Data.List.Types (List(..), (:))
import Data.String (joinWith)
import Data.Tuple (Tuple(..))

import System.Random.Mersenne (seed, int32)
import System.Random.Mersenne.Types (MTState)

main :: Eff (console :: CONSOLE, exception :: EXCEPTION) Unit
main = do
  timer <- time "generation"
  let ints = randomInts 100 1000000
  timeEnd timer
  log $ "generated list of " <> show (length ints) <> " random ints."

  log "comparing to python"
  logShow $ compareToPython 100 100000
  logShow $ compareToPython 0 100000
  logShow $ compareToPython top 100000

  where
    randomInts :: Int -> Int -> List Int
    randomInts s length = reverse $ go (seed s) Nil 0
      where
        go :: MTState -> List Int -> Int -> List Int
        go state accum index -- tail recursive, builds backward
          | (index < length) =
            case int32 state
              of (Tuple i state') -> go state' (i : accum) (index + 1)
          | otherwise = accum

    compareToPython :: Int -> Int -> Boolean
    compareToPython s count = randomIntsStr s count == pythonRandomInts s count

    randomIntsStr :: Int -> Int -> String
    randomIntsStr s count =
      "[" <> joinWith ", " (fromFoldable $ show <$> randomInts s count) <> "]"

    pythonRandomInts :: Int -> Int -> String
    pythonRandomInts s count =
      exec "python" ["test/mersenne.py", show s, show count]

foreign import exec :: String -> Array String -> String
