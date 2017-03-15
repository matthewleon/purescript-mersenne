module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Now (NOW, now)
import Data.Array (fromFoldable)
import Data.DateTime.Instant (unInstant)
import Data.List.Lazy (take)
import Data.List.Lazy.Types (List)
import Data.Maybe (Maybe(..))
import Data.Unfoldable (unfoldr)

import System.Random.Mersenne (seed, int32)

main :: Eff (now :: NOW, console :: CONSOLE) Unit
main = do
  start <- unInstant <$> now
  let ints = fromFoldable $ take 100000 (lazyRandomInts 100)
  generated <- unInstant <$> now
  logShow $ generated - start
  where
    lazyRandomInts :: Int -> List Int
    lazyRandomInts i = unfoldr (Just <<< int32) (seed i)
