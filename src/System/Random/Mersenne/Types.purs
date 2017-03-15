module System.Random.Mersenne.Types (MTState(..), MTBlock(..), Seed(..)) where

import Data.Monoid ((<>))
import Data.Show (class Show, show)

data MTState = MTState MTBlock Int
instance showMTState :: Show MTState where
  show (MTState blk i) = "(MTState " <> show blk <> " " <> show i <> ")"

newtype MTBlock = MTBlock (Array Int)
instance showMTBlock :: Show MTBlock where
  show (MTBlock arr) = "(MTBlock " <> show arr <> ")"

newtype Seed = Seed Int
