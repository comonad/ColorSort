{-# LANGUAGE ViewPatterns #-}

module FreeMonoid (
  freeMonoid, FreeMonoid(NIL), Foldable.toList
) where


import Data.Function
import Data.Foldable as Foldable



freeMonoid :: [a] -> FreeMonoid a
freeMonoid = foldr CONS NIL

data FreeMonoid a = NIL | CONS a (FreeMonoid a) | APPEND (FreeMonoid a) (FreeMonoid a) | SNOC (FreeMonoid a) a
    deriving Functor
instance Semigroup (FreeMonoid a) where
    NIL<>bs = bs
    (CONS a NIL)<>bs = CONS a bs
    (SNOC NIL a)<>bs = CONS a bs
    as<>NIL = as
    as<>(CONS b NIL) = SNOC as b
    as<>(SNOC NIL b) = SNOC as b
    as<>bs = APPEND as bs
instance Monoid (FreeMonoid a) where
    mempty = NIL
instance Eq a => Eq (FreeMonoid a) where
    (==) = (==) `on` Foldable.toList
instance Ord a => Ord (FreeMonoid a) where
    compare = compare `on` Foldable.toList
instance Show a => Show (FreeMonoid a) where
    show = show . Foldable.toList
instance Applicative FreeMonoid where
    pure a = CONS a NIL
    --(<*>) :: f (a -> b) -> f a -> f b
    (<*>) fab fa = do
        ab <- fab
        ab <$> fa
instance Monad FreeMonoid where
    (>>=) NIL f = NIL
    (>>=) (CONS a bs) f = (f a) <> (bs >>= f)
    (>>=) (APPEND as bs) f = (as >>= f) <> (bs >>= f)
    (>>=) (SNOC as b) f = (as >>= f) <> (f b)

instance Foldable FreeMonoid where
    --foldr :: (a -> b -> b) -> b -> Monoid a -> b
    foldr (<) nil m = m << nil
        where
        NIL << nil = nil
        (CONS a bs) << nil = a < (bs << nil)
        (APPEND as bs) << nil = as << (bs << nil)
        (SNOC as b) << nil = as << (b < nil)







