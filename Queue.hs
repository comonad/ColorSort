{-# LANGUAGE ViewPatterns #-}

module Queue where

import Data.List as List
import Control.Monad as Monad
import Control.Monad.Trans.Maybe as Maybe
import Data.Maybe (listToMaybe,fromJust,catMaybes)
import Control.Concurrent

import Data.IORef
import Data.Function
import Data.Set as Set
import Data.Foldable as Foldable



data Queue a = Queue ![a] ![[a]] ![[a]] deriving Functor
pushBack :: Queue a -> [a] -> Queue a
pushBack q [] = q
pushBack (Queue as ass bss) bs = Queue as ass (bs:bss)
popFront :: Queue a -> Maybe (a,Queue a)
popFront (Queue (a:as) ass bss) = Just (a,Queue as ass bss)
popFront (Queue [] ((a:as):ass) bss) = Just (a,Queue as ass bss)
popFront (Queue [] [] []) = Nothing
popFront (Queue [] [] (List.reverse->((a:as):ass))) = Just (a,Queue as ass [])
listToQueue :: [a] -> Queue a
listToQueue as = Queue as [] []
queueToList :: Queue a -> [a]
queueToList (Queue as ass []) = mconcat (as:ass)
queueToList (Queue as ass bss) = List.foldr (<>) (mconcat $ List.reverse bss) (as:ass)
instance Semigroup (Queue a) where
   q <> (Queue [] [] []) = q
   (Queue [] [] []) <> q = q
   (Queue as ass bss1) <> (Queue [] [] bss2) = (Queue as ass (bss1<>bss2))
   (Queue as1 ass1 []) <> (Queue as2 ass2 bss2) = (Queue as1 (ass1<>(as2:ass2)) bss2)
   q1 <> q2 = q1 `pushBack` Foldable.toList q2
instance Monoid (Queue a) where
    mempty = (Queue [] [] [])
instance Foldable Queue where
    foldr cons nil = f
        where f queue = case popFront queue of
                            Nothing -> nil
                            Just (a,as) -> a `cons` (f as)
    toList = queueToList
instance Eq a => Eq (Queue a) where
    (==) = (==) `on` Foldable.toList
instance Ord a => Ord (Queue a) where
    compare = compare `on` Foldable.toList
instance Show a => Show (Queue a) where
    show = show . Foldable.toList



