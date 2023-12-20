{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PackageImports #-}


module ParallelMonoid (
  ParallelMonoid(), parToList, parToListS, parToList',
  Foldable.null, Foldable.toList
) where

import Data.Function
import Data.Foldable as Foldable
import Data.Traversable as Traversable
import qualified "parallel" Control.Parallel.Strategies as P



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
    (<*>) fab fa = AP fab fa

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







