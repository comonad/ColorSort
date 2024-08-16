{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}


module Bottle where

import Data.List as List
import Control.Monad as Monad
import Control.Monad.Trans.Maybe as Maybe
import Data.Maybe (listToMaybe,fromJust,catMaybes,maybeToList)
import Control.Concurrent
import Data.Bits
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




type Liquid = (Int,Color)
--data Bottle = Bottle_
--    { bottleLiquids :: [Liquid]
--    , bottleFillLevel :: !Int
--    } deriving (Eq,Ord)
newtype Bottle = Bottle_ Integer deriving (Eq,Ord)

makeBottle :: [Color] -> Bottle
makeBottle cs = List.foldr bottle_stackLiquid (Bottle_ 0) liquids
    where
        liquids :: [Liquid]
        liquids = [(List.length g,c)|g@(c:_)<-List.group cs]

bottleFillLevel :: Bottle -> Int
bottleFillLevel (Bottle_ b) = fromIntegral $ b .&. 0x3f

pack_liquid :: Liquid -> Integer
pack_liquid (n,Color_ c) = fromIntegral $ (n`shiftL`6).|. c
unpack_liquid :: Integer -> Liquid
unpack_liquid (fromIntegral->i) = ((i `shiftR` 6) .&. 0x3f,Color_ $ i .&. 0x3f)

bottle_stackLiquid :: Liquid -> Bottle -> Bottle
bottle_stackLiquid li@(n,_) bottle@(Bottle_ b) =
    let bottleFillLevel' = fromIntegral $ bottleFillLevel bottle + n
     in Bottle_ $ ((((b `shiftR` 6) `shiftL` 12) .|. pack_liquid li) `shiftL` 6) .|. bottleFillLevel'

bottle_popLiquid :: Bottle -> Maybe(Liquid,Bottle)
bottle_popLiquid (Bottle_ 0) = Nothing
bottle_popLiquid bottle@(Bottle_ b) =
    let li@(n,_) = unpack_liquid (b `shiftR` 6)
        bottleFillLevel' = fromIntegral $ bottleFillLevel bottle - n
     in Just (li,Bottle_ $ ((b `shiftR` 18)`shiftL` 6).|. bottleFillLevel')



bottleDropN :: Int -> Bottle -> Bottle
bottleDropN 0 bottle = error "bottleDropN 0"
bottleDropN n bottle = case bottle_popLiquid bottle of
    Nothing -> error "bottleDropN _ emptyBottle"
    Just ((k,c),b) | k<n -> error "bottleDropN toomuch liquid"
    Just ((k,c),b) | k==n -> b
    Just ((k,c),b) | k>n -> bottle_stackLiquid (k-n,c) b

bottleFillN :: Int -> Color -> Bottle -> Bottle
bottleFillN 0 color bottle = error "bottleFillN 0"
bottleFillN n color bottle = case bottle_popLiquid bottle of
    Just ((k,c),b) | c==color -> bottle_stackLiquid (k+n,color) b
    _ -> bottle_stackLiquid (n,color) bottle


bottleTopLiquid :: Bottle -> Maybe Liquid
bottleTopLiquid bottle = fst <$> bottle_popLiquid bottle
bottleTopColor :: Bottle -> Maybe Color
bottleTopColor bottle = snd <$> bottleTopLiquid bottle
bottleUniColor :: Bottle -> Maybe Color
bottleUniColor bottle = do
    case bottle_popLiquid bottle of
        Just ((n,c),_) | n==bottleFillLevel bottle -> Just c
        _ -> Nothing
--bottleFillLevel :: Bottle -> Int

bottleFreeSpace :: Bottle -> Int
bottleFreeSpace bottle = bottleMaxHeight - bottleFillLevel bottle
bottleIsEmpty :: Bottle -> Bool
bottleIsEmpty bottle = bottleFillLevel bottle == 0
bottleIsFull :: Bottle -> Bool
bottleIsFull bottle = bottleFillLevel bottle == bottleMaxHeight
bottleIsUnicolor :: Color -> Bottle -> Bool
bottleIsUnicolor color bottle = bottleUniColor bottle == Just color
bottleIsComplete :: Bottle -> Bool
bottleIsComplete bottle = (fst<$>bottleTopLiquid bottle) == Just bottleMaxHeight

bottleTransferFromTo :: (Bottle,Bottle) -> Maybe (Bottle,Bottle)
bottleTransferFromTo (fromBottle,toBottle) = do
    (n,color)<-bottleTopLiquid fromBottle
    let k = bottleFreeSpace toBottle
    let t = n `min` k
    guard (t>0)
    guard $ bottleIsEmpty toBottle || bottleTopColor toBottle == Just color
    return (bottleDropN t fromBottle, bottleFillN t color toBottle)


