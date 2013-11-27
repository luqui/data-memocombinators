module Data.MemoCombinators.Class 
    ( MemoTable(..)
    , Memoizable(..)
    ) where

import qualified Data.MemoCombinators as Memo
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import Data.Ratio (Ratio, numerator, denominator, (%))
import Control.Arrow ((&&&))

-- | The class of types which have complete memo tables.
class MemoTable a where
    table :: Memo.Memo a

instance MemoTable Bool where table = Memo.bool
instance MemoTable Char where table = Memo.char
instance MemoTable Int  where table = Memo.integral
instance MemoTable Int8 where table = Memo.integral
instance MemoTable Int16 where table = Memo.integral
instance MemoTable Int32 where table = Memo.integral
instance MemoTable Int64 where table = Memo.integral
instance MemoTable Integer where table = Memo.integral
instance MemoTable Ordering where table = Memo.enum
instance MemoTable Word where table = Memo.integral
instance MemoTable Word8 where table = Memo.integral
instance MemoTable Word16 where table = Memo.integral
instance MemoTable Word32 where table = Memo.integral
instance MemoTable Word64 where table = Memo.integral

instance MemoTable () where table = Memo.unit
instance (MemoTable a, MemoTable b) => MemoTable (a,b) where
    table = uncurry . memoize . curry
instance (MemoTable a, MemoTable b, MemoTable c) => MemoTable (a,b,c) where
    table f = \(a,b,c) -> m a b c
        where m = memoize (\a b c -> f (a,b,c))
instance (MemoTable a, MemoTable b, MemoTable c, MemoTable d) => MemoTable (a,b,c,d) where
    table f = \(a,b,c,d) -> m a b c d
        where m = memoize (\a b c d -> f (a,b,c,d))
instance (MemoTable a, MemoTable b, MemoTable c, MemoTable d, MemoTable e) => MemoTable (a,b,c,d,e) where
    table f = \(a,b,c,d,e) -> m a b c d e
        where m = memoize (\a b c d e -> f (a,b,c,d,e))
instance (MemoTable a, MemoTable b, MemoTable c, MemoTable d, MemoTable e, MemoTable f) => MemoTable (a,b,c,d,e,f) where
    table f = \(a,b,c,d,e,f') -> m a b c d e f'
        where m = memoize (\a b c d e f' -> f (a,b,c,d,e,f'))
instance (MemoTable a, MemoTable b, MemoTable c, MemoTable d, MemoTable e, MemoTable f, MemoTable g) => MemoTable (a,b,c,d,e,f,g) where
    table f = \(a,b,c,d,e,f',g) -> m a b c d e f' g
        where m = memoize (\a b c d e f' g -> f (a,b,c,d,e,f',g))
instance (MemoTable a, MemoTable b, MemoTable c, MemoTable d, MemoTable e, MemoTable f, MemoTable g, MemoTable h) => MemoTable (a,b,c,d,e,f,g,h) where
    table f = \(a,b,c,d,e,f',g,h) -> m a b c d e f' g h
        where m = memoize (\a b c d e f' g h -> f (a,b,c,d,e,f',g,h))
instance (MemoTable a, MemoTable b, MemoTable c, MemoTable d, MemoTable e, MemoTable f, MemoTable g, MemoTable h, MemoTable i) => MemoTable (a,b,c,d,e,f,g,h,i) where
    table f = \(a,b,c,d,e,f',g,h,i) -> m a b c d e f' g h i
        where m = memoize (\a b c d e f' g h i -> f (a,b,c,d,e,f',g,h,i))
instance (MemoTable a, MemoTable b, MemoTable c, MemoTable d, MemoTable e, MemoTable f, MemoTable g, MemoTable h, MemoTable i, MemoTable j) => MemoTable (a,b,c,d,e,f,g,h,i,j) where
    table f = \(a,b,c,d,e,f',g,h,i,j) -> m a b c d e f' g h i j
        where m = memoize (\a b c d e f' g h i j -> f (a,b,c,d,e,f',g,h,i,j))
instance (MemoTable a, MemoTable b, MemoTable c, MemoTable d, MemoTable e, MemoTable f, MemoTable g, MemoTable h, MemoTable i, MemoTable j, MemoTable k) => MemoTable (a,b,c,d,e,f,g,h,i,j,k) where
    table f = \(a,b,c,d,e,f',g,h,i,j,k) -> m a b c d e f' g h i j k
        where m = memoize (\a b c d e f' g h i j k -> f (a,b,c,d,e,f',g,h,i,j,k))
instance (MemoTable a, MemoTable b, MemoTable c, MemoTable d, MemoTable e, MemoTable f, MemoTable g, MemoTable h, MemoTable i, MemoTable j, MemoTable k, MemoTable l) => MemoTable (a,b,c,d,e,f,g,h,i,j,k,l) where
    table f = \(a,b,c,d,e,f',g,h,i,j,k,l) -> m a b c d e f' g h i j k l
        where m = memoize (\a b c d e f' g h i j k l -> f (a,b,c,d,e,f',g,h,i,j,k,l))
instance (MemoTable a, MemoTable b, MemoTable c, MemoTable d, MemoTable e, MemoTable f, MemoTable g, MemoTable h, MemoTable i, MemoTable j, MemoTable k, MemoTable l, MemoTable m) => MemoTable (a,b,c,d,e,f,g,h,i,j,k,l,m) where
    table f = \(a,b,c,d,e,f',g,h,i,j,k,l,m') -> m a b c d e f' g h i j k l m'
        where m = memoize (\a b c d e f' g h i j k l m' -> f (a,b,c,d,e,f',g,h,i,j,k,l,m'))
instance (MemoTable a, MemoTable b, MemoTable c, MemoTable d, MemoTable e, MemoTable f, MemoTable g, MemoTable h, MemoTable i, MemoTable j, MemoTable k, MemoTable l, MemoTable m, MemoTable n) => MemoTable (a,b,c,d,e,f,g,h,i,j,k,l,m,n) where
    table f = \(a,b,c,d,e,f',g,h,i,j,k,l,m',n) -> m a b c d e f' g h i j k l m' n
        where m = memoize (\a b c d e f' g h i j k l m' n -> f (a,b,c,d,e,f',g,h,i,j,k,l,m',n))
instance (MemoTable a, MemoTable b, MemoTable c, MemoTable d, MemoTable e, MemoTable f, MemoTable g, MemoTable h, MemoTable i, MemoTable j, MemoTable k, MemoTable l, MemoTable m, MemoTable n, MemoTable o) => MemoTable (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) where
    table f = \(a,b,c,d,e,f',g,h,i,j,k,l,m',n,o) -> m a b c d e f' g h i j k l m' n o
        where m = memoize (\a b c d e f' g h i j k l m' n o -> f (a,b,c,d,e,f',g,h,i,j,k,l,m',n,o))


instance (MemoTable a) => MemoTable [a] where table = Memo.list table
instance (MemoTable a) => MemoTable (Maybe a) where table = Memo.maybe table
instance (Integral a, MemoTable a) => MemoTable (Ratio a) where
    table = Memo.wrap (uncurry (%)) (numerator &&& denominator) table


-- | The class of functions which can be completely memoized.
class Memoizable a where
    memoize :: a -> a

instance (MemoTable a) => Memoizable (a -> b) where
    memoize = table
