
module Conc.Tree
  ( Conc
  , empty
  , singleton
  , null
  , level
  , size
  , fromList
  , toList
  , lookup
  , update
  , insert
  , append
  , concat
  ) where

import           Prelude (($), Traversable)
import qualified Prelude as P

import           Conc.Tree.Buffer
import           Conc.Tree.Internal

import           Control.Monad.ST (runST)
import           Control.Monad    (forM_)

import qualified Data.Foldable      as F

toList :: Conc a -> [a]
toList = F.toList

fromList :: Traversable t => t a -> Conc a
fromList xs = runST $ do
  buf <- newBuffer (P.length xs)
  forM_ xs (push buf)
  toConc buf

