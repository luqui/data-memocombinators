------------------------------------------------
-- |
-- Module    : Data.MemoCombinators
-- Copyright : (c) Luke Palmer 2008-2010
-- License   : BSD3
--
-- Maintainer : Luke Palmer <lrpalmer@gmail.com>
-- Stability  : experimental
--
-- This module provides combinators for building memo tables
-- over various data types, so that the type of table can
-- be customized depending on the application.
--
-- This module is designed to be imported /qualified/, eg.
--
-- > import qualified Data.MemoCombinators as Memo
--
-- Usage is straightforward: apply an object of type @Memo a@
-- to a function of type @a -> b@, and get a memoized function
-- of type @a -> b@.  For example:
--
-- > fib = Memo.integral fib'
-- >    where
-- >    fib' 0 = 0
-- >    fib' 1 = 1
-- >    fib' x = fib (x-1) + fib (x-2)
------------------------------------------------

module Data.MemoCombinators
    ( Memo
    , wrap
    , memo2, memo3, memoSecond, memoThird
    , bool, char, list, boundedList, either, maybe, unit, pair
    , enum, integral, bits
    , switch
    , RangeMemo
    , arrayRange, unsafeArrayRange, chunks
    )
where

import Prelude hiding (either, maybe)
import Data.Bits
import qualified Data.Array as Array
import Data.Char (ord,chr)
import qualified Data.IntTrie as IntTrie

-- | The type of a memo table for functions of type @a@.
type Memo a = forall r. (a -> r) -> (a -> r)

-- | Given a memoizer for @a@ and an isomorphism between @a@ and @b@, build
-- a memoizer for @b@.
wrap :: (a -> b) -> (b -> a) -> Memo a -> Memo b
wrap i j m f = m (f . i) . j

-- | Memoize a two argument function (just apply the table directly for
-- single argument functions).
memo2 :: Memo a -> Memo b -> (a -> b -> r) -> (a -> b -> r)
memo2 a b = a . (b .)

-- | Memoize a three argument function.
memo3 :: Memo a -> Memo b -> Memo c -> (a -> b -> c -> r) -> (a -> b -> c -> r)
memo3 a b c = a . (memo2 b c .)

-- | Memoize the second argument of a function.
memoSecond :: Memo b -> (a -> b -> r) -> (a -> b -> r)
memoSecond b = (b .)

-- | Memoize the third argument of a function.
memoThird :: Memo c -> (a -> b -> c -> r) -> (a -> b -> c -> r)
memoThird c = (memoSecond c .)

bool :: Memo Bool
bool f = cond (f True) (f False)
    where
    cond t f True  = t
    cond t f False = f

list :: Memo a -> Memo [a]
list m f = table (f []) (m (\x -> list m (f . (x:))))
    where
    table nil cons [] = nil
    table nil cons (x:xs) = cons x xs

char :: Memo Char
char = wrap chr ord integral

-- | Build a table which memoizes all lists of less than the given length.
boundedList :: Int -> Memo a -> Memo [a]
boundedList 0 m f = f
boundedList n m f = table (f []) (m (\x -> boundedList (n-1) m (f . (x:))))
    where
    table nil cons [] = nil
    table nil cons (x:xs) = cons x xs

either :: Memo a -> Memo b -> Memo (Either a b)
either m m' f = table (m (f . Left)) (m' (f . Right))
    where
    table l r (Left x) = l x
    table l r (Right x) = r x

maybe :: Memo a -> Memo (Maybe a)
maybe m f = table (f Nothing) (m (f . Just))
    where
    table n j Nothing = n
    table n j (Just x) = j x

unit :: Memo ()
unit f = let m = f () in \() -> m

pair :: Memo a -> Memo b -> Memo (a,b)
pair m m' f = uncurry (m (\x -> m' (\y -> f (x,y))))

-- | Memoize an enum type.
enum :: (Enum a) => Memo a
enum = wrap toEnum fromEnum integral

-- | Memoize an integral type.
integral :: (Integral a) => Memo a
integral = wrap fromInteger toInteger bits

-- | Memoize an ordered type with a bits instance.
bits :: (Num a, Ord a, Bits a) => Memo a
bits f = IntTrie.apply (fmap f IntTrie.identity)

-- | @switch p a b@ uses the memo table @a@ whenever @p@ gives
-- true and the memo table @b@ whenever @p@ gives false.
switch :: (a -> Bool) -> Memo a -> Memo a -> Memo a
switch p m m' f = table (m f) (m' f)
    where
    table t f x | p x       = t x
                | otherwise = f x

-- | The type of builders for ranged tables; takes a lower bound and an upper
-- bound, and returns a memo table for that range.
type RangeMemo a = (a,a) -> Memo a

-- | Build a memo table for a range using a flat array.  If items are
-- given outside the range, don't memoize.
arrayRange :: (Array.Ix a) => RangeMemo a
arrayRange rng = switch (Array.inRange rng) (unsafeArrayRange rng) id

-- | Build a memo table for a range using a flat array.  If items are
-- given outside the range, behavior is undefined.
unsafeArrayRange :: (Array.Ix a) => RangeMemo a
unsafeArrayRange rng f = (Array.listArray rng (map f (Array.range rng)) Array.!)


-- | Given a list of ranges, (lazily) build a memo table for each one
-- and combine them using linear search.
chunks :: (Array.Ix a) => RangeMemo a -> [(a,a)] -> Memo a
chunks rmemo cs f = lookup (cs `zip` map (\rng -> rmemo rng f) cs)
    where
    lookup [] _ = error "Element not in table"
    lookup ((r,c):cs) x | Array.inRange r x = c x
                        | otherwise = lookup cs x
