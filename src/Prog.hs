{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PackageImports #-}


module Prog (
    Prog(), guardHistory, foreach, with, runProg, raisePrio
) where

import Data.List as List
import Control.Monad as Monad
import Control.Monad.Trans.Maybe as Maybe
import Data.Maybe (listToMaybe,fromJust,catMaybes,maybeToList)
import Control.Concurrent
import Control.Applicative

import Data.IORef
import Data.Function
import Data.Set as Set
import Data.Map.Strict as Map
import Data.Foldable as Foldable

import qualified "parallel" Control.Parallel.Strategies as P

import ParallelMonoid
--import FreeMonoid
--type ParallelMonoid = FreeMonoid

data Prog h a = Pure a
              | forall b. Bind (Prog h b) (b -> Prog h a)
              | Spawn [Prog h a]
              | JoinOn h (Prog h a)
              | RaisePrio Double (Prog h a)


guardHistory :: h -> Prog h ()
guardHistory h = JoinOn h (Pure ())
foreach :: [a] -> Prog h a
foreach as = Spawn $ fmap Pure as
with :: Maybe a -> Prog h a
with Nothing = Spawn []
with (Just a) = Pure a
raisePrio :: Double -> Prog h ()
raisePrio prio = RaisePrio prio (Pure ())


runBind1 :: Prog h a -> (a -> Prog h b) -> Prog h b
runBind1 (Pure a) a_pb = a_pb a
runBind1 (Bind (pc :: Prog h c) (c_pa :: c -> Prog h a)) a_pb = pc `runBind1` (\c -> c_pa c `runBind1` a_pb)
runBind1 (JoinOn h cont) a_pb = JoinOn h (cont `runBind1` a_pb)
runBind1 (Spawn pas) a_pb = Spawn [ runBind1 pa a_pb | pa <- pas ]
runBind1 (RaisePrio prio pa) a_pb = RaisePrio prio (runBind1 pa a_pb)


data Assembly h a
    = AResult a
    | AJoin h (ParallelMonoid(Assembly h a))
    | ARaisePrio Double (ParallelMonoid(Assembly h a))
assemble :: Prog h a -> ParallelMonoid(Assembly h a)
assemble (Pure a) = pure (AResult a)
assemble (Bind pa a_pb) = assemble (runBind1 pa a_pb)
assemble (Spawn ps) = mconcat $ assemble <$> ps
assemble (JoinOn h cont) = pure (AJoin h (assemble cont))
assemble (RaisePrio prio cont) = pure (ARaisePrio prio (assemble cont))

runAssembly :: Ord h => Set h -> [Assembly h a] -> [a]
runAssembly history as = runAssembly' 0 mempty mempty history as
    where
    runAssembly' :: Ord h
                 => Double
                 -> Map Double (ParallelMonoid (Assembly h a))
                 -> ParallelMonoid (Assembly h a)
                 -> Set h
                 -> [Assembly h a]
                 -> [a]
    runAssembly' prio priobs bs history (AResult a:as) = a:runAssembly' prio priobs bs history as
    runAssembly' prio priobs bs history (AJoin h a:as)
        | Set.member h history = runAssembly' prio priobs bs history as
        | otherwise            = runAssembly' prio priobs (bs<>a) (Set.insert h history) as
    runAssembly' prio priobs bs history (ARaisePrio raise a:as)
        | raise <= 0 = runAssembly' prio (Map.insertWith(<>) (prio+raise)(a) priobs) bs history as
        | raise > 0 = runAssembly' (prio+raise) (Map.insertWith(<>) (prio)(ParallelMonoid.fromList as<>bs) priobs) a history []
    runAssembly' prio priobs bs history [] | Foldable.null bs = case Map.maxViewWithKey priobs of
      Nothing -> []
      Just ((prio,bs),priobs) -> runAssembly' prio priobs mempty history (parToListS bs)
    runAssembly' prio priobs bs history [] = runAssembly' prio priobs mempty history (parToListS bs)

runProg :: (Ord h) => Prog h a -> [a]
runProg p = runAssembly Set.empty  $ Foldable.toList (assemble p)


deriving instance Functor (Prog h)
instance Applicative (Prog h) where
    --pure :: a -> f a
    pure = Pure
    --(<*>) :: f (a -> b) -> f a -> f b
    (<*>) fab fa = do
        ab <- fab
        a <- fa
        return (ab a)
instance Monad (Prog h) where
    --(>>=) :: m a -> (a -> m b) -> m b
    (>>=) (Pure a) f = f a
    (>>=) ma f = Bind ma f
instance Alternative (Prog h) where
    empty = Spawn []
    (<|>) a b = Spawn [a,b]
instance MonadPlus (Prog h)









