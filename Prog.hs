{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PackageImports #-}


module Prog (
    Prog(), guardHistory, foreach, runProg
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
import Data.Foldable as Foldable

import qualified "parallel" Control.Parallel.Strategies as P

import FreeMonoid


data Prog h a = Pure a
              | forall b. Bind (Prog h b) (b -> Prog h a)
              | Spawn [Prog h a]
              | JoinOn h (Prog h a)


guardHistory :: h -> Prog h ()
guardHistory h = JoinOn h (Pure ())
foreach :: [a] -> Prog h a
foreach as = Spawn $ fmap Pure as
--with :: Maybe a -> Prog h a
--with Nothing = Spawn []
--with (Just a) = Pure a



runBind :: Prog h a -> Prog h a
runBind (Bind pb b_pa) = case pb of
                            (Pure a) -> runBind (b_pa a)
                            (Bind (pc :: Prog h c) (c_pb :: c -> Prog h b)) -> runBind (pc >>= (c_pb >=> b_pa))
                            (JoinOn h cont) -> (JoinOn h $ runBind (cont >>= b_pa))
                            (Spawn pbs) -> (Spawn $ fmap (runBind . (>>=b_pa)) pbs)
runBind x = x


data Assembly h a = AResult a | AJoin h (FreeMonoid(Assembly h a))
assemble :: Prog h a -> FreeMonoid(Assembly h a)
assemble (Pure a) = pure (AResult a)
assemble p@(Bind _ _) = assemble (runBind p)
assemble (Spawn ps) = mconcat $ assemble <$> ps
assemble (JoinOn h cont) = pure (AJoin h (assemble cont))

runAssembly :: Ord h => Set h -> [Assembly h a] -> [a]
runAssembly history as = runAssembly' NIL history as
    where
    runAssembly' :: Ord h => FreeMonoid (Assembly h a) -> Set h -> [Assembly h a] -> [a]
    runAssembly' bs history (AResult a:as) = a:runAssembly' bs history as
    runAssembly' bs history (AJoin h a:as) = if Set.member h history then runAssembly' bs history as else runAssembly' (bs<>a) (Set.insert h history) as
    runAssembly' NIL history [] = []
    runAssembly' bs history [] = runAssembly' NIL history (P.withStrategy (P.parList P.rseq) $ Foldable.toList bs)

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









