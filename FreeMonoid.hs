{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFunctor #-}


module FreeMonoid (
  freeMonoid, FreeMonoid(NIL),
  pattern CONS, pattern SNOC, pattern SINGLETON, pattern APPEND,
  unCONS, unSNOC, unSINGLETON, unAPPEND,
  Foldable.toList
) where


import Data.Function
import Data.Foldable as Foldable



pattern CONS x xs <- (unCONS -> Just(x,xs)) where
    CONS x xs = CONS_ x xs
pattern SNOC xs x <- (unSNOC -> Just(xs,x)) where
    SNOC xs x = SNOC_ xs x

pattern SINGLETON x <- (unSINGLETON -> Just x) where
    SINGLETON x = CONS_ x NIL
pattern APPEND as bs <- (unAPPEND -> Just (as,bs)) where
    APPEND as bs = APPEND_ as bs



freeMonoid :: [a] -> FreeMonoid a
freeMonoid = foldr CONS_ NIL

unCONS :: FreeMonoid a -> Maybe(a,FreeMonoid a)
unCONS NIL = Nothing
unCONS (CONS_ a bs) = Just(a,bs)
unCONS (APPEND_ (unCONS -> Just(a,ar)) bs) = Just(a,ar<>bs)
unCONS (SNOC_ NIL b) = Just(b,NIL)
unCONS (SNOC_ (unCONS -> Just(a,ar)) b) = Just(a,SNOC_ ar b)
unSNOC :: FreeMonoid a -> Maybe(FreeMonoid a,a)
unSNOC NIL = Nothing
unSNOC (CONS_ a NIL) = Just(NIL,a)
unSNOC (CONS_ a (unSNOC -> Just(br,b))) = Just(CONS_ a br,b)
unSNOC (APPEND_ as (unSNOC -> Just(br,b))) = Just(as<>br,b)
unSNOC (SNOC_ as b) = Just(as,b)

unSINGLETON :: FreeMonoid a -> Maybe a
unSINGLETON (CONS_ a NIL) = Just a
unSINGLETON (SNOC_ NIL b) = Just b
unSINGLETON _ = Nothing
unAPPEND :: FreeMonoid a -> Maybe(FreeMonoid a,FreeMonoid a)
unAPPEND NIL = Nothing
unAPPEND (CONS_ _ NIL) = Nothing
unAPPEND (SNOC_ NIL _) = Nothing
unAPPEND (APPEND_ as bs) = Just(as,bs)
unAPPEND (CONS_ a bs) = Just(SINGLETON a,bs)
unAPPEND (SNOC_ as b) = Just(as,SINGLETON b)

data FreeMonoid a
    = NIL
    | CONS_ a (FreeMonoid a)
    | APPEND_ (FreeMonoid a) (FreeMonoid a) -- ^ both sides are non-empty
    | SNOC_ (FreeMonoid a) a
    deriving Functor

instance Semigroup (FreeMonoid a) where
    NIL<>bs = bs
    (CONS_ a NIL)<>bs = CONS_ a bs
    (SNOC_ NIL a)<>bs = CONS_ a bs
    as<>NIL = as
    as<>(CONS_ b NIL) = SNOC_ as b
    as<>(SNOC_ NIL b) = SNOC_ as b
    as<>bs = APPEND_ as bs
instance Monoid (FreeMonoid a) where
    mempty = NIL
instance Eq a => Eq (FreeMonoid a) where
    (==) = (==) `on` Foldable.toList
instance Ord a => Ord (FreeMonoid a) where
    compare = compare `on` Foldable.toList
instance Show a => Show (FreeMonoid a) where
    show = show . Foldable.toList
instance Applicative FreeMonoid where
    pure a = CONS_ a NIL
    --(<*>) :: f (a -> b) -> f a -> f b
    (<*>) fab fa = do
        ab <- fab
        ab <$> fa
instance Monad FreeMonoid where
    (>>=) NIL f = NIL
    (>>=) (CONS_ a bs) f = (f a) <> (bs >>= f)
    (>>=) (APPEND_ as bs) f = (as >>= f) <> (bs >>= f)
    (>>=) (SNOC_ as b) f = (as >>= f) <> (f b)

instance Foldable FreeMonoid where
    --foldr :: (a -> b -> b) -> b -> Monoid a -> b
    foldr (<) nil m = m << nil
        where
        NIL << nil = nil
        (CONS_ a bs) << nil = a < (bs << nil)
        (APPEND_ as bs) << nil = as << (bs << nil)
        (SNOC_ as b) << nil = as << (b < nil)







