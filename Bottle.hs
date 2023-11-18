{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}


module Bottle where

import Data.List as List
import Control.Monad as Monad
import Control.Monad.Trans.Maybe as Maybe
import Data.Maybe (listToMaybe,fromJust,catMaybes,maybeToList)
import Control.Concurrent

import Data.IORef
import Data.Function
import Data.Set as Set
import Data.Foldable as Foldable

import FreeMonoid


bottleMaxHeight = 4 :: Int

newtype Color = Color_ Int deriving (Eq,Ord)
[ash,blue,earth,green,lila,mint,orange,pink,red,sky,weed,yellow] = Color_ <$> [1..12]
instance Show Color where
    show (Color_ c) = List.words "ash blue earth green lila mint orange pink red sky weed yellow" !! (c-1)



newtype Bottle = Bottle_ [Color] deriving (Eq,Ord)
makeBottle :: [Color] -> Bottle
makeBottle = Bottle_






type Liquid = (Int,Color)
bottleTopLiquid :: Bottle -> Maybe Liquid
bottleTopLiquid bottle@(Bottle_ bottle_) = do
    color <- bottleTopColor bottle
    return (List.length $ List.takeWhile(==color) bottle_,color)
bottleTopColor :: Bottle -> Maybe Color
bottleTopColor (Bottle_ bottle) = do
    (color:_) <- Just bottle
    return color
bottleUniColor :: Bottle -> Maybe Color
bottleUniColor (Bottle_ bottle) = do
    (color:_) <- Just bottle
    guard $ List.all (==color) bottle
    return color
bottleFillLevel :: Bottle -> Int
bottleFillLevel (Bottle_ bottle) = List.length bottle
bottleFreeSpace :: Bottle -> Int
bottleFreeSpace bottle = bottleMaxHeight - bottleFillLevel bottle
bottleIsEmpty :: Bottle -> Bool
bottleIsEmpty (Bottle_ bottle) = List.null bottle
bottleIsFull :: Bottle -> Bool
bottleIsFull bottle = bottleFillLevel bottle == bottleMaxHeight
bottleIsUnicolor :: Color -> Bottle -> Bool
bottleIsUnicolor color (Bottle_ []) = False
bottleIsUnicolor color (Bottle_ bottle) = List.all (==color) bottle
bottleIsComplete :: Bottle -> Bool
bottleIsComplete bottle = (fst<$>bottleTopLiquid bottle) == Just bottleMaxHeight
bottleDropN :: Int -> Bottle -> Bottle
bottleDropN n (Bottle_ bottle) = Bottle_ $ List.drop n bottle
bottleFillN :: Int -> Color -> Bottle -> Bottle
bottleFillN n color (Bottle_ bottle) = Bottle_ $ List.replicate n color <> bottle
bottleTransferFromTo :: (Bottle,Bottle) -> Maybe (Bottle,Bottle)
bottleTransferFromTo (fromBottle,toBottle) = do
    (n,color)<-bottleTopLiquid fromBottle
    let k = bottleFreeSpace toBottle
    let t = n `min` k
    guard (t>0)
    guard $ bottleIsEmpty toBottle || bottleTopColor toBottle == Just color
    return (bottleDropN t fromBottle, bottleFillN t color toBottle)


