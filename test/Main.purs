module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Array (fromFoldable)
import Data.List.Lazy (take)
import Data.List.Lazy.Types (List)
import Data.Maybe (Maybe(..))
import Data.Unfoldable (unfoldr)

import System.Random.Mersenne (seed, int32)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = logShow <<< fromFoldable $ take 100 (lazyRandomInts 100)
  where
    lazyRandomInts :: Int -> List Int
    lazyRandomInts i = unfoldr (Just <<< int32) (seed i)
