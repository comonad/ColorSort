{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}


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



makeBottle :: [Color] -> Bottle
makeBottle cs = Bottle_ [(List.length g,c)|g@(c:_)<-List.group cs] (List.length cs)

type Liquid = (Int,Color)
data Bottle = Bottle_
    { bottleLiquids :: [Liquid]
    , bottleFillLevel :: !Int
    } deriving (Eq,Ord)



bottleTopLiquid :: Bottle -> Maybe Liquid
bottleTopLiquid bottle = listToMaybe $ bottleLiquids bottle
bottleTopColor :: Bottle -> Maybe Color
bottleTopColor bottle = snd <$> bottleTopLiquid bottle
bottleUniColor :: Bottle -> Maybe Color
bottleUniColor bottle = do
    case bottleLiquids bottle of
        [(_,c)] -> Just c
        _ -> Nothing
--bottleFillLevel :: Bottle -> Int
--bottleFillLevel (Bottle_ bottle) = List.length bottle
bottleFreeSpace :: Bottle -> Int
bottleFreeSpace bottle = bottleMaxHeight - bottleFillLevel bottle
bottleIsEmpty :: Bottle -> Bool
bottleIsEmpty bottle = bottleFillLevel bottle == 0
bottleIsFull :: Bottle -> Bool
bottleIsFull bottle = bottleFillLevel bottle == bottleMaxHeight
bottleIsUnicolor :: Color -> Bottle -> Bool
bottleIsUnicolor color bottle = bottleUniColor bottle == Just color
bottleIsComplete :: Bottle -> Bool
bottleIsComplete bottle = (fst<$>bottleLiquids bottle) == [bottleMaxHeight]
bottleDropN :: Int -> Bottle -> Bottle
bottleDropN 0 bottle = error "bottleDropN 0"
bottleDropN n (Bottle_{bottleLiquids=((k,c):ls),bottleFillLevel})|k<n =
    bottleDropN (n-k) Bottle_{bottleLiquids=ls,bottleFillLevel=bottleFillLevel-k}
bottleDropN n (Bottle_{bottleLiquids=((k,c):ls),bottleFillLevel})|k==n =
    Bottle_{bottleLiquids=ls,bottleFillLevel=bottleFillLevel-n}
bottleDropN n (Bottle_{bottleLiquids=((k,c):ls),bottleFillLevel})|k>n =
    Bottle_{bottleLiquids=(k-n,c):ls,bottleFillLevel=bottleFillLevel-n}
bottleFillN :: Int -> Color -> Bottle -> Bottle
bottleFillN 0 color bottle = error "bottleFillN 0"
bottleFillN n color (Bottle_{bottleLiquids=((k,c):ls),bottleFillLevel}) | c==color =
    Bottle_{bottleLiquids=(n+k,color):ls,bottleFillLevel=bottleFillLevel+n}
bottleFillN n color (Bottle_{bottleLiquids,bottleFillLevel}) =
    Bottle_{bottleLiquids=(n,color):bottleLiquids,bottleFillLevel=bottleFillLevel+n}



bottleTransferFromTo :: (Bottle,Bottle) -> Maybe (Bottle,Bottle)
bottleTransferFromTo (fromBottle,toBottle) = do
    (n,color)<-bottleTopLiquid fromBottle
    let k = bottleFreeSpace toBottle
    let t = n `min` k
    guard (t>0)
    guard $ bottleIsEmpty toBottle || bottleTopColor toBottle == Just color
    return (bottleDropN t fromBottle, bottleFillN t color toBottle)


