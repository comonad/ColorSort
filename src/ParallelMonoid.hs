{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PackageImports #-}


module ParallelMonoid (
  ParallelMonoid(), parToList, parToListS, parToList',
--  pattern CONS, pattern SNOC, pattern SINGLETON, pattern APPEND,
--  unCONS, unSNOC, unSINGLETON, unAPPEND,
  Foldable.null, Foldable.toList
) where


-- TODO: build on NIL,SINGLETON,APPEND instead of CONS and SNOC

import Data.Function
import Data.Foldable as Foldable
import Data.Traversable as Traversable
import qualified "parallel" Control.Parallel.Strategies as P


{-
pattern CONS x xs <- (unCONS -> Just(x,xs)) where
    CONS x xs = CONS_ x xs
pattern SNOC xs x <- (unSNOC -> Just(xs,x)) where
    SNOC xs x = SNOC_ xs x

pattern SINGLETON x <- (unSINGLETON -> Just x) where
    SINGLETON x = CONS_ x NIL
pattern APPEND as bs <- (unAPPEND -> Just (as,bs)) where
    APPEND as bs = as <> bs

-}


rparWith = (P.parEval .)
parList strat = traverse (rparWith strat)

parStrat :: (a -> P.Eval b) -> (ParallelMonoid a) -> P.Eval (ParallelMonoid b)
parStrat f NIL = return NIL
parStrat f (SINGLETON a) = SINGLETON <$> f a
parStrat f (MAP xa px) = parStrat (f . xa) px
parStrat f (APPEND pa pb) = do
    pa' <- rparWith (parStrat f) pa
    pb' <- rparWith (parStrat f) pb
    return (APPEND pa' pb')
parStrat f (L1 as) = L1 <$> rparWith (parList f) as
parStrat f (JOIN ppa) = JOIN <$> parStrat (rparWith (parStrat f)) ppa
parStrat f (AP pxa px) = do
    pxa' <- rparWith (parStrat P.r0) pxa
    px' <- rparWith (parStrat P.r0) px
    runMAPm f (AP pxa' px')


parToList,parToListS,parToList' :: ParallelMonoid a -> [a]
parToList pa = Foldable.toList $ P.runEval (parStrat P.r0 pa)
parToListS pa = Foldable.toList $ P.runEval (parStrat P.rseq pa)
parToList' pa = Foldable.toList $ P.runEval (parStrat P.rpar pa)

data ParallelMonoid a
    = NIL
    | SINGLETON a
    | forall x. MAP (x->a) (ParallelMonoid x)
    | APPEND (ParallelMonoid a) (ParallelMonoid a)
    | L1 [a]
    -- -| L2 [ParallelMonoid a]
    | JOIN (ParallelMonoid (ParallelMonoid a))
    | forall x. AP (ParallelMonoid (x->a)) (ParallelMonoid x)



runMAPm :: Applicative m => (a->m b) -> ParallelMonoid a -> m (ParallelMonoid b)
runMAPm f NIL = pure NIL
runMAPm f (SINGLETON a) = SINGLETON <$> f a
runMAPm f (MAP xa px) = runMAPm (f.xa) px
runMAPm f (APPEND pa pb) = APPEND <$> runMAPm f pa <*> runMAPm f pb
runMAPm f (L1 as) = L1 <$> traverse f as
runMAPm f (JOIN ppa) = JOIN <$> runMAPm (runMAPm f) ppa
runMAPm f (AP xa px) = JOIN <$> runMAPm (runMAPm f) ((<$> px) <$> xa)

instance Traversable ParallelMonoid where
    --traverse :: Applicative f => (a -> f b) -> ParallelMonoid a -> f (ParallelMonoid b)
    traverse = runMAPm


instance Functor ParallelMonoid where
    --fmap f NIL = NIL
    --fmap f (SINGLETON a) = SINGLETON (f a)
    --fmap f (MAP xa x) = MAP (f . xa) x
    fmap f pa = MAP f pa


instance Semigroup (ParallelMonoid a) where
    (<>) = APPEND
instance Monoid (ParallelMonoid a) where
    mempty = NIL
instance Eq a => Eq (ParallelMonoid a) where
    (==) = (==) `on` Foldable.toList
instance Ord a => Ord (ParallelMonoid a) where
    compare = compare `on` Foldable.toList
instance Show a => Show (ParallelMonoid a) where
    show = show . Foldable.toList
instance Applicative ParallelMonoid where
    pure = SINGLETON
    --(<*>) :: f (a -> b) -> f a -> f b
    --(<*>) fab fa = JOIN ((<$> fa) <$> fab)
    (<*>) fab fa = AP fab fa
    --(<*>) fab fa = L2 [ ab <$> fa | ab <- Foldable.toList fab ]

instance Monad ParallelMonoid where
    (>>=) NIL f = NIL
    (>>=) (SINGLETON a) f = f a
    (>>=) pa f = JOIN (MAP f pa)


instance Foldable ParallelMonoid where
    --null :: ParallelMonoid a -> Bool
    null NIL = True
    null (SINGLETON _) = False
    null (MAP _ px) = null px
    null (APPEND pa pb) = null pa && null pb
    null (L1 []) = True
    null (L1 _) = False
    null (JOIN ppa) = all null ppa
    null (AP pxa px) = null pxa || null px
    --foldr :: (a -> b -> b) -> b -> Monoid a -> b
    foldr (<) nil m = m << nil
        where
        NIL << nil = nil
        (SINGLETON a) << nil = a < nil
        (MAP f px) << nil = foldr ((<) . f) nil px
        (APPEND as bs) << nil = as << (bs << nil)
        (L1 as) << nil = foldr (<) nil as
        (JOIN ppa) << nil = foldr (<<) nil ppa
        (AP xa px) << nil = JOIN ((<$> px) <$> xa) << nil







