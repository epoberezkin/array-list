{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : Data.Array.IsList
-- Copyright   : (c) Evgeny Poberezkin
-- License     : MIT
--
-- Maintainer  : evgeny@poberezkin.com
-- Stability   : experimental
-- Portability : non-portable
--
-- This package provides "orphan" 'IsList' instances for 'Array's
-- with `Integral` indices up to 5 dimensions to allow initializing
-- 'Array's from [nested] lists using 'OverloadedLists' GHC extension.
--
-- It also includes more generic 'arrayNestedList' and 'toNestedList'
-- functions to convert between nested lists and 'Array's with any indices.
--
-- __Examples__:
--
-- >>> ["one","two","three"] :: Array Int String
-- array (0,2) [(0,"one"),(1,"two"),(2,"three")]
--
-- >>> [[0,1,2], [10,11,12]] :: Array (Int, Int) Int
-- array ((0,0),(1,2)) [((0,0),0),((0,1),1),((0,2),2),((1,0),10),((1,1),11),((1,2),12)]
--
-- If any of the nested lists contains smaller number of elements
-- than the first nested list in the same dimension,
-- the array creation will fail.
--
-- >>> [[1,2],[3]] :: Array (Int, Int) Int
-- ... Exception: (Array.!): undefined array element
--
-- Nested lists with larger number of elements will be truncated.
module Data.Array.IsList
  ( -- * IsList
    IsList,
    fromList,
    toList,

    -- * ArrayNestedList
    ArrayNestedList,
    arrayNestedList,
    toNestedList,
  )
where

import Data.Array
import Data.Int
import Data.List
import Data.Word
import GHC.Exts
import Numeric.Natural

instance IsList (Array Int e) where
  type Item (Array Int e) = e
  fromList = genericFromList
  toList = elems

instance IsList (Array Int8 e) where
  type Item (Array Int8 e) = e
  fromList = genericFromList
  toList = elems

instance IsList (Array Int16 e) where
  type Item (Array Int16 e) = e
  fromList = genericFromList
  toList = elems

instance IsList (Array Int32 e) where
  type Item (Array Int32 e) = e
  fromList = genericFromList
  toList = elems

instance IsList (Array Int64 e) where
  type Item (Array Int64 e) = e
  fromList = genericFromList
  toList = elems

instance IsList (Array Integer e) where
  type Item (Array Integer e) = e
  fromList = genericFromList
  toList = elems

instance IsList (Array Natural e) where
  type Item (Array Natural e) = e
  fromList = genericFromList
  toList = elems

instance IsList (Array Word e) where
  type Item (Array Word e) = e
  fromList = genericFromList
  toList = elems

instance IsList (Array Word8 e) where
  type Item (Array Word8 e) = e
  fromList = genericFromList
  toList = elems

instance IsList (Array Word16 e) where
  type Item (Array Word16 e) = e
  fromList = genericFromList
  toList = elems

instance IsList (Array Word32 e) where
  type Item (Array Word32 e) = e
  fromList = genericFromList
  toList = elems

instance IsList (Array Word64 e) where
  type Item (Array Word64 e) = e
  fromList = genericFromList
  toList = elems

genericFromList :: (Integral i, Ix i) => [e] -> Array i e
genericFromList xs = listArray (0, top xs) xs

top :: Integral i => [e] -> i
top l = genericLength l - 1

instance
  (Integral i, Integral j, Ix i, Ix j) =>
  IsList (Array (i, j) e)
  where
  type Item (Array (i, j) e) = [e]
  fromList :: [[e]] -> Array (i, j) e
  fromList l = arrayNestedList bnds l
    where
      bnds = ((0, 0), (top l, top $ head l))
  toList :: Array (i, j) e -> [[e]]
  toList = toNestedList

instance
  (Integral i, Integral j, Integral k, Ix i, Ix j, Ix k) =>
  IsList (Array (i, j, k) e)
  where
  type Item (Array (i, j, k) e) = [[e]]
  fromList :: [[[e]]] -> Array (i, j, k) e
  fromList l = arrayNestedList bnds l
    where
      (h1, h2) = (head l, head h1)
      bnds = ((0, 0, 0), (top l, top h1, top h2))
  toList :: Array (i, j, k) e -> [[[e]]]
  toList = toNestedList

instance
  (Integral i, Integral j, Integral k, Integral m, Ix i, Ix j, Ix k, Ix m) =>
  IsList (Array (i, j, k, m) e)
  where
  type Item (Array (i, j, k, m) e) = [[[e]]]
  fromList :: [[[[e]]]] -> Array (i, j, k, m) e
  fromList l = arrayNestedList bnds l
    where
      (h1, h2, h3) = (head l, head h1, head h2)
      bnds = ((0, 0, 0, 0), (top l, top h1, top h2, top h3))
  toList :: Array (i, j, k, m) e -> [[[[e]]]]
  toList = toNestedList

instance
  (Integral i, Integral j, Integral k, Integral m, Integral n, Ix i, Ix j, Ix k, Ix m, Ix n) =>
  IsList (Array (i, j, k, m, n) e)
  where
  type Item (Array (i, j, k, m, n) e) = [[[[e]]]]
  fromList :: [[[[[e]]]]] -> Array (i, j, k, m, n) e
  fromList l = arrayNestedList bnds l
    where
      (h1, h2, h3, h4) = (head l, head h1, head h2, head h3)
      bnds = ((0, 0, 0, 0, 0), (top l, top h1, top h2, top h3, top h4))
  toList :: Array (i, j, k, m, n) e -> [[[[[e]]]]]
  toList = toNestedList

-- | 'ArrayNestedList' class defines methods to convert between
-- nested lists and multi-dimensional (up to 5) 'Array's with any indices,
-- not only 'Integral', using provided range of indices.
class Ix i => ArrayNestedList i e where
  type NestedList i e

  -- | Converts nested list to multi-dimensional 'Array'
  -- Similarly to 'arrayList' function, it does not require to pass index
  -- for each element, only the range of indices.
  arrayNestedList :: (i, i) -> NestedList i e -> Array i e

  -- | Converts multi-dimensional 'Array' to nested list.
  toNestedList :: Array i e -> NestedList i e

instance (Ix i, Ix j) => ArrayNestedList (i, j) e where
  type NestedList (i, j) e = [[e]]
  arrayNestedList ::
    ((i, j), (i, j)) -> [[e]] -> Array (i, j) e
  arrayNestedList bnds@((l1, l2), (r1, r2)) l =
    array
      bnds
      [ ((i, j), x)
        | xs <- l,
          x <- xs
        | i <- range (l1, r1),
          j <- range (l2, r2)
      ]
  toNestedList :: Array (i, j) e -> [[e]]
  toNestedList arr =
    let ((_, l2), (_, r2)) = bounds arr
     in splitList (l2, r2) $ elems arr

splitList :: Ix i => (i, i) -> [e] -> [[e]]
splitList = split' . rangeSize
  where
    split' _ [] = []
    split' n xs =
      let (part, rest) = splitAt n xs
       in part : split' n rest

instance (Ix i, Ix j, Ix k) => ArrayNestedList (i, j, k) e where
  type NestedList (i, j, k) e = [[[e]]]
  arrayNestedList ::
    ((i, j, k), (i, j, k)) -> [[[e]]] -> Array (i, j, k) e
  arrayNestedList bnds@((l1, l2, l3), (r1, r2, r3)) l =
    array
      bnds
      [ ((i, j, k), x)
        | xss <- l,
          xs <- xss,
          x <- xs
        | i <- range (l1, r1),
          j <- range (l2, r2),
          k <- range (l3, r3)
      ]
  toNestedList :: Array (i, j, k) e -> [[[e]]]
  toNestedList arr =
    let ((_, l2, l3), (_, r2, r3)) = bounds arr
     in splitList (l2, r2)
          $ splitList (l3, r3)
          $ elems arr

instance (Ix i, Ix j, Ix k, Ix m) => ArrayNestedList (i, j, k, m) e where
  type NestedList (i, j, k, m) e = [[[[e]]]]
  arrayNestedList ::
    ((i, j, k, m), (i, j, k, m)) ->
    [[[[e]]]] ->
    Array (i, j, k, m) e
  arrayNestedList bnds@((l1, l2, l3, l4), (r1, r2, r3, r4)) l =
    array
      bnds
      [ ((i, j, k, m), x)
        | xsss <- l,
          xss <- xsss,
          xs <- xss,
          x <- xs
        | i <- range (l1, r1),
          j <- range (l2, r2),
          k <- range (l3, r3),
          m <- range (l4, r4)
      ]
  toNestedList :: Array (i, j, k, m) e -> [[[[e]]]]
  toNestedList arr =
    let ((_, l2, l3, l4), (_, r2, r3, r4)) = bounds arr
     in splitList (l2, r2)
          $ splitList (l3, r3)
          $ splitList (l4, r4)
          $ elems arr

instance (Ix i, Ix j, Ix k, Ix m, Ix n) => ArrayNestedList (i, j, k, m, n) e where
  type NestedList (i, j, k, m, n) e = [[[[[e]]]]]
  arrayNestedList ::
    ((i, j, k, m, n), (i, j, k, m, n)) ->
    [[[[[e]]]]] ->
    Array (i, j, k, m, n) e
  arrayNestedList bnds@((l1, l2, l3, l4, l5), (r1, r2, r3, r4, r5)) l =
    array
      bnds
      [ ((i, j, k, m, n), x)
        | xssss <- l,
          xsss <- xssss,
          xss <- xsss,
          xs <- xss,
          x <- xs
        | i <- range (l1, r1),
          j <- range (l2, r2),
          k <- range (l3, r3),
          m <- range (l4, r4),
          n <- range (l5, r5)
      ]
  toNestedList :: Array (i, j, k, m, n) e -> [[[[[e]]]]]
  toNestedList arr =
    let ((_, l2, l3, l4, l5), (_, r2, r3, r4, r5)) = bounds arr
     in splitList (l2, r2)
          $ splitList (l3, r3)
          $ splitList (l4, r4)
          $ splitList (l5, r5)
          $ elems arr
