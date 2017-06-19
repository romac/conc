{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

module Conc.Tree
  ( Conc
  , empty
  , singleton
  , null
  , level
  , size
  , lookup
  , update
  -- , insert
  , append
  , concat
  ) where

import           Prelude                 hiding (concat, lookup, null)

import           Control.DeepSeq         (NFData)
import           Control.Monad           (when)
import           Control.Monad.Primitive (PrimMonad, PrimState, RealWorld)
import           Control.Monad.ST        (ST)
import           Data.STRef              (STRef, newSTRef, readSTRef, writeSTRef, modifySTRef)
import           Data.IORef              (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import           Data.Data               (Data)
import           Data.Monoid             ((<>))
import           Data.Semigroup          (Semigroup)
import           Data.Typeable           (Typeable)
import           GHC.Generics            (Generic)

import qualified Data.Vector             as V
import qualified Data.Vector.Mutable     as M

-- | A Conc Tree [1][2] is a data-structure that stores element sequences,
-- | and provides amortized /O(1)/ time append and prepend operations,
-- | /O(log n)/ time insert and remove operations and /O(log n)/ time concatenation.
-- | This data structure is particularly viable for functional task-parallel and data-parallel programming.
data Conc a
  = Empty
  | Leaf !(L a)
  | Conc a :<>: Conc a
  | Append (Conc a) (Conc a)
  deriving -- TODO: Applicative, Monad, IsList, Alternative, MonadPlus
    ( Eq, Ord, Show, Read
    , Functor, Foldable, Traversable
    , Data, Typeable, Generic, NFData
    )

data L a
  = Single a
  | Chunk (V.Vector a) Int Int
  deriving
    ( Eq, Ord, Show, Read
    , Functor, Foldable, Traversable
    , Data, Typeable, Generic, NFData
    )

instance Monoid (Conc a) where
  mempty  = empty
  mappend = concat

instance Semigroup (Conc a) where

-- | /O(1)/. The empty Conc-Tree.
empty :: Conc a
empty = Empty
{-# INLINE empty #-}

-- | /O(1)/. A singleton Conc-Tree.
singleton :: a -> Conc a
singleton = Leaf . Single
{-# INLINE singleton #-}

-- | /O(1)/. Is this the empty Conc-Tree?
null :: Conc a -> Bool
null Empty = True
null _     = False
{-# INLINE null #-}

-- | /O(log(n)/. The number of levels in this Conc-Tree.
level :: Conc a -> Int
level (ls :<>: rs)   = 1 + level ls `max` level rs
level (Append ls rs) = 1 + level ls `max` level rs
level _              = 0

-- | /O(log(n)/. The number of elements in this Conc-Tree.
size :: Conc a -> Int
size (ls :<>: rs)         = size ls + size rs
size (Append ls rs)       = size ls + size rs
size (Leaf (Chunk _ n _)) = n
size (Leaf (Single _))    = 1
size Empty                = 0

-- | /O(log(n)/. Lookup an element at a specified index.
lookup :: Conc a -> Int -> Maybe a
lookup _                     i | i < 0       = Nothing
lookup (ls :<>: _)           i | i < size ls = lookup ls i
lookup (ls :<>: rs)          i               = lookup rs (i - size ls)
lookup (Leaf (Chunk xs n _)) i               = onlyIf (i < n) $ xs V.! i
lookup (Leaf (Single x))     0               = Just x
lookup _                     _               = Nothing

-- | /O(log(n)/. Update an element at a specified index.
update :: Conc a -> Int -> a -> Maybe (Conc a)
update _                     i _ | i < 0       = Nothing
update (ls :<>: rs)          i x | i < size ls = (:<>: rs) <$> update ls i x
update (ls :<>: rs)          i x               = (ls :<>:) <$> update rs (i - size ls) x
update (Leaf (Single _))     _ x               = Just (Leaf (Single x))
update (Leaf (Chunk xs n k)) i x               = onlyIf (i < n) $ Leaf (Chunk (V.unsafeUpd xs [(i, x)]) n k)
update _                     _ _               = Nothing

-- TODO: insert :: Conc a -> Int -> a -> Conc a


-- | /O(1)/ (ephemeral). Add an element to the rs end of the Conc-Tree.
append :: Conc a -> a -> Conc a
append c x = append_ c (Single x)

-- TODO: L a is ugly, fix this
append_ :: Conc a -> L a -> Conc a
append_ Empty           ys = Leaf ys
append_ xs@(Leaf _)     ys = xs :<>: Leaf ys
append_ xs@(_ :<>: _)   ys = Append xs (Leaf ys)
append_ xs@(Append _ _) ys = append' xs (Leaf ys)

-- FIXME: Make this typesafe
append' :: Conc a -> Conc a -> Conc a
append' xs@(Append _ rs) ys | level rs > level ys = Append xs ys
append' xs@(Append ls rs) ys = let zs = rs :<>: ys in case ls of
  ws@(Append _ _) -> append' ws zs
  ws | level ws <= level xs -> append' ws zs
  ws | otherwise -> Append ws zs

append' _ _ = error "internal error: append' expects an Append"

-- | /O(log(n)/. Concatenate two Conc-Trees.
concat :: Conc a -> Conc a -> Conc a
concat Empty ys = ys
concat xs Empty = xs
concat xs ys    = normalize xs `conc` normalize ys

conc :: Conc a -> Conc a -> Conc a
conc = undefined

normalize :: Conc a -> Conc a
normalize (Append ls rs) = wrap ls rs
normalize xs                  = xs
{-# INLINE normalize #-}

wrap :: Conc a -> Conc a -> Conc a
wrap (Append ws zs) ys = wrap ws (zs <> ys)
wrap xs ys             = xs <> ys

onlyIf :: Bool -> a -> Maybe a
onlyIf True a  = Just a
onlyIf False _ = Nothing

class PrimMonad m => PrimMonadRef m where
  type PrimRef m :: * -> *

  newRef    :: a -> m (PrimRef m a)
  readRef   :: PrimRef m a -> m a
  writeRef  :: PrimRef m a -> a -> m ()
  modifyRef :: PrimRef m a -> (a -> a) -> m ()

instance PrimMonadRef (ST s) where
  type PrimRef (ST s) = STRef s

  newRef    = newSTRef
  readRef   = readSTRef
  writeRef  = writeSTRef
  modifyRef = modifySTRef

instance PrimMonadRef IO where
  type PrimRef IO = IORef

  newRef    = newIORef
  readRef   = readIORef
  writeRef  = writeIORef
  modifyRef = modifyIORef

data ConcBuffer s r a
  = ConcBuffer
  { _maxSize  :: r Int
  , _lastSize :: r Int
  , _conc     :: r (Conc a)
  , _buffer   :: M.MVector s a
  }

type IOConcBuffer   = ConcBuffer RealWorld
type STConcBuffer s = ConcBuffer s

newBuffer :: PrimMonadRef m => Int -> m (ConcBuffer (PrimState m) (PrimRef m) a)
newBuffer k = do
  _maxSize <- newRef k
  _lastSize <- newRef 0
  _conc <- newRef empty
  _buffer <- M.new k
  pure $ ConcBuffer {..}

push :: PrimMonadRef m => ConcBuffer (PrimState m) (PrimRef m) a -> a -> m ()
push buf@ConcBuffer{..} x = do
  maxSize <- readRef _maxSize
  size <- readRef _lastSize
  when (size >= maxSize) (expand buf)
  lastSize <- readRef _lastSize
  M.write _buffer lastSize x
  modifyRef _lastSize (+1)

expand :: PrimMonadRef m => ConcBuffer (PrimState m) (PrimRef m) a -> m ()
expand ConcBuffer{..} = do
  maxSize <- readRef _maxSize
  lastSize <- readRef _lastSize
  buffer <- V.freeze _buffer
  modifyRef _conc $ \c -> append' c (Leaf (Chunk buffer lastSize maxSize))
  writeRef _lastSize 0
  -- No need to reset _buffer, as it is going to be overwritten

