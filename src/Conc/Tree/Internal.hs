
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}

module Conc.Tree.Internal
  ( Conc(..)
  , Leaf(..)
  , empty
  , singleton
  , null
  , level
  , size
  , lookup
  , update
  , insert
  , append
  , appendLeaf
  , concat
  ) where

import           Prelude         hiding (concat, lookup, null)

import           Control.DeepSeq (NFData)
import           Data.Data       (Data)
import           Data.Monoid     ((<>))
import           Data.Semigroup  (Semigroup)
import           Data.Typeable   (Typeable)
import           GHC.Generics    (Generic)

import qualified Data.Vector     as V

-- | A Conc Tree is a data-structure that stores element sequences,
--   and provides amortized /O(1)/ time append and prepend operations,
--   /O(log n)/ time insert and remove operations and /O(log n)/ time concatenation.
--   This data structure is particularly viable for functional task-parallel and data-parallel programming.
data Conc a
  = Empty
  | Leaf !(Leaf a)
  | Conc a :<>: Conc a
  | Append (Conc a) (Conc a)
  deriving -- TODO: Applicative, Monad, IsList, Alternative, MonadPlus
    ( Eq, Ord, Show, Read
    , Functor, Foldable, Traversable
    , Data, Typeable, Generic, NFData
    )

data Leaf a
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
lookup (ls :<>: rs)          i = lookup rs (i - size ls)
lookup (Leaf (Chunk xs n _)) i = onlyIf (i < n) $ xs V.! i
lookup (Leaf (Single x))     0 = Just x
lookup _                     _ = Nothing

-- | /O(log(n)/. Update an element at a specified index.
update :: Conc a -> Int -> a -> Maybe (Conc a)
update _                     i _ | i < 0       = Nothing
update (ls :<>: rs)          i x | i < size ls = (:<>: rs) <$> update ls i x
update (ls :<>: rs)          i x = (ls :<>:) <$> update rs (i - size ls) x
update (Leaf (Single _))     _ x = Just (Leaf (Single x))
update (Leaf (Chunk xs n k)) i x = onlyIf (i < n) $ Leaf (Chunk (V.unsafeUpd xs [(i, x)]) n k)
update _                     _ _ = Nothing

-- TODO
insert :: Conc a -> Int -> a -> Conc a
insert = undefined

-- | /O(1)/ (ephemeral). Add an element to the rs end of the Conc-Tree.
append :: Conc a -> a -> Conc a
append c x = appendLeaf c (Single x)

appendLeaf :: Conc a -> Leaf a -> Conc a
appendLeaf Empty           ys = Leaf ys
appendLeaf xs@(Leaf _)     ys = xs :<>: Leaf ys
appendLeaf xs@(_ :<>: _)   ys = Append xs (Leaf ys)
appendLeaf xs@(Append _ _) ys = append' xs (Leaf ys)

-- FIXME: Make this typesafe
append' :: Conc a -> Conc a -> Conc a
append' xs@(Append _ rs) ys | level rs > level ys =
  Append xs ys

append' xs@(Append ls rs) ys =
  let zs = rs :<>: ys
   in case ls of
        ws@(Append _ _) -> append' ws zs
        ws              | level ws <= level xs -> append' ws zs
        ws              | otherwise -> Append ws zs

append' _ _ = error "internal error: append' expects an Append"

-- | /O(log(n)/. Concatenate two Conc-Trees.
concat :: Conc a -> Conc a -> Conc a
concat Empty ys = ys
concat xs Empty = xs
concat xs ys    = normalize xs `conc` normalize ys
{-# INLINABLE concat #-}

conc :: Conc a -> Conc a -> Conc a
conc = undefined

normalize :: Conc a -> Conc a
normalize (Append ls rs) = wrap ls rs
normalize xs             = xs
{-# INLINE normalize #-}

wrap :: Conc a -> Conc a -> Conc a
wrap (Append ws zs) ys = wrap ws (zs <> ys)
wrap xs ys             = xs <> ys
{-# INLINABLE wrap #-}

onlyIf :: Bool -> a -> Maybe a
onlyIf True a  = Just a
onlyIf False _ = Nothing
{-# INLINE onlyIf #-}

