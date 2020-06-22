{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
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
-- __Examples__:
--
-- >>> ["one","two","three"] :: Array Int String
-- array (0,2) [(0,"one"),(1,"two"),(2,"three")]
--
-- >>> [[0,1,2], [10,11,12]] :: Array (Int, Int) Int
-- array ((0,0),(1,2)) [((0,0),0),((0,1),1),((0,2),2),((1,0),10),((1,1),11),((1,2),12)]
--
-- If any of the nested lists contains different number of elements
-- for the same dimension, the array creation will fail.
--
-- >>> [[1],[2,3]] :: Array (Int, Int) Int
-- array *** Exception: Error in array index
module Data.Array.IsList
  ( IsList,
    fromList,
    toList,
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

splitList :: Integral i => i -> i -> [e] -> [[e]]
splitList _ _ [] = []
splitList l r xs =
  let (part, rest) = genericSplitAt (r - l + 1) xs
   in part : splitList l r rest

top :: Integral i => [e] -> i
top l = genericLength l - 1

h1 :: [e] -> e
h1 = head

h2 :: [[e]] -> e
h2 = head . head

h3 :: [[[e]]] -> e
h3 = head . h2

h4 :: [[[[e]]]] -> e
h4 = head . h3

inxd :: Integral i => [e] -> [(i, e)]
inxd = zip [0 ..]

instance
  (Integral i, Integral j, Ix i, Ix j) =>
  IsList (Array (i, j) e)
  where
  type Item (Array (i, j) e) = [e]
  fromList :: [[e]] -> Array (i, j) e
  fromList l =
    array
      ((0, 0), (top l, top $ h1 l))
      [ ((i, j), x)
        | (i, xs) <- inxd l,
          (j, x) <- inxd xs
      ]
  toList :: Array (i, j) e -> [[e]]
  toList arr =
    let ((_, l), (_, r)) = bounds arr
     in splitList l r $ elems arr

instance
  (Integral i, Integral j, Integral k, Ix i, Ix j, Ix k) =>
  IsList (Array (i, j, k) e)
  where
  type Item (Array (i, j, k) e) = [[e]]
  fromList :: [[[e]]] -> Array (i, j, k) e
  fromList l =
    array
      ((0, 0, 0), (top l, top $ h1 l, top $ h2 l))
      [ ((i, j, k), x)
        | (i, xss) <- inxd l,
          (j, xs) <- inxd xss,
          (k, x) <- inxd xs
      ]
  toList :: Array (i, j, k) e -> [[[e]]]
  toList arr =
    let ((_, l1, l2), (_, r1, r2)) = bounds arr
     in splitList l1 r1
          $ splitList l2 r2
          $ elems arr

instance
  ( Integral i,
    Integral j,
    Integral k,
    Integral m,
    Ix i,
    Ix j,
    Ix k,
    Ix m
  ) =>
  IsList (Array (i, j, k, m) e)
  where
  type Item (Array (i, j, k, m) e) = [[[e]]]
  fromList :: [[[[e]]]] -> Array (i, j, k, m) e
  fromList l =
    array
      ( (0, 0, 0, 0),
        (top l, top $ h1 l, top $ h2 l, top $ h3 l)
      )
      [ ((i, j, k, m), x)
        | (i, xsss) <- inxd l,
          (j, xss) <- inxd xsss,
          (k, xs) <- inxd xss,
          (m, x) <- inxd xs
      ]
  toList :: Array (i, j, k, m) e -> [[[[e]]]]
  toList arr =
    let ((_, l1, l2, l3), (_, r1, r2, r3)) = bounds arr
     in splitList l1 r1
          $ splitList l2 r2
          $ splitList l3 r3
          $ elems arr

instance
  ( Integral i,
    Integral j,
    Integral k,
    Integral m,
    Integral n,
    Ix i,
    Ix j,
    Ix k,
    Ix m,
    Ix n
  ) =>
  IsList (Array (i, j, k, m, n) e)
  where
  type Item (Array (i, j, k, m, n) e) = [[[[e]]]]
  fromList :: [[[[[e]]]]] -> Array (i, j, k, m, n) e
  fromList l =
    array
      ( (0, 0, 0, 0, 0),
        (top l, top $ h1 l, top $ h2 l, top $ h3 l, top $ h4 l)
      )
      [ ((i, j, k, m, n), x)
        | (i, xssss) <- inxd l,
          (j, xsss) <- inxd xssss,
          (k, xss) <- inxd xsss,
          (m, xs) <- inxd xss,
          (n, x) <- inxd xs
      ]
  toList :: Array (i, j, k, m, n) e -> [[[[[e]]]]]
  toList arr =
    let ((_, l1, l2, l3, l4), (_, r1, r2, r3, r4)) = bounds arr
     in splitList l1 r1
          $ splitList l2 r2
          $ splitList l3 r3
          $ splitList l4 r4
          $ elems arr
