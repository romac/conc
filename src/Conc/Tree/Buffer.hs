{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}

module Conc.Tree.Buffer
  ( ConcBuffer
  , type IOConcBuffer
  , type STConcBuffer
  , newBuffer
  , push
  , toConc
  ) where

import           Control.Monad           (when)
import           Control.Monad.Primitive (PrimMonad, PrimState, RealWorld)
import           Data.Primitive.MutVar   (MutVar, modifyMutVar', newMutVar, readMutVar, writeMutVar)

import qualified Data.Vector             as V
import qualified Data.Vector.Mutable     as M

import           Conc.Tree.Internal

data ConcBuffer s a
  = ConcBuffer
  { _maxSize  :: MutVar s Int
  , _lastSize :: MutVar s Int
  , _conc     :: MutVar s (Conc a)
  , _buffer   :: M.MVector s a
  }

type IOConcBuffer   = ConcBuffer RealWorld
type STConcBuffer s = ConcBuffer s

newBuffer :: PrimMonad m => Int -> m (ConcBuffer (PrimState m) a)
newBuffer k = do
  _maxSize <- newMutVar k
  _lastSize <- newMutVar 0
  _conc <- newMutVar empty
  _buffer <- M.new k
  pure $ ConcBuffer {..}

push :: PrimMonad m => ConcBuffer (PrimState m) a -> a -> m ()
push buf@ConcBuffer{..} x = do
  maxSize <- readMutVar _maxSize
  size <- readMutVar _lastSize
  when (size >= maxSize) (expand buf)
  lastSize <- readMutVar _lastSize
  M.write _buffer lastSize x
  modifyMutVar' _lastSize (+1)

expand :: PrimMonad m => ConcBuffer (PrimState m) a -> m ()
expand ConcBuffer{..} = do
  maxSize <- readMutVar _maxSize
  lastSize <- readMutVar _lastSize
  buffer <- V.freeze _buffer
  modifyMutVar' _conc $ \c -> appendLeaf c (Chunk buffer lastSize maxSize)
  writeMutVar _lastSize 0
  -- No need to reset _buffer, as it is going to be overwritten

toConc :: PrimMonad m => ConcBuffer (PrimState m) a -> m (Conc a)
toConc buf = do
  expand buf
  conc <- readMutVar (_conc buf)
  pure $ normalize conc

