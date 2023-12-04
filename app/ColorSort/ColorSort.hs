{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}


module Main where

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

import Bottle
import Prog

type Level = [Bottle]
level141 :: Level
level141 = makeBottle <$>
  [[mint,ash,sky,orange]
  ,[orange,orange,pink,yellow]
  ,[yellow,orange,red,green]
  ,[lila,lila,blue,sky]
  ,[yellow,green,earth,blue]
  ,[red,blue,pink,weed]
  ,[sky,earth,blue,earth]
  ,[weed,pink,ash,sky]
  ,[pink,weed,red,lila]
  ,[yellow,mint,weed,red]
  ,[ash,ash,earth,mint]
  ,[mint,lila,green,green]
  ,[],[]
  ]
level843 :: Level
level843 = makeBottle <$>
  [[ash,green,sky,weed]
  ,[red,pink,green,lila]
  ,[mint,earth,ash,blue]
  ,[sky,weed,sky,earth]
  ,[ash,orange,orange,orange]
  ,[red,weed,earth,earth]
  ,[blue,yellow,red,mint]
  ,[red,yellow,orange,ash]
  ,[lila,yellow,mint,green]
  ,[lila,sky,yellow,blue]
  ,[weed,blue,mint,pink]
  ,[pink,pink,green,lila]
  ,[],[]
  ]
level877 :: Level
level877 = makeBottle <$>
  [[ash,yellow,blue,pink]
  ,[orange,sky,lila,orange]
  ,[pink,green,green,sky]
  ,[ash,weed,lila,red]
  ,[weed,mint,mint,red]
  ,[red,yellow,sky,earth]
  ,[yellow,orange,sky,weed]
  ,[earth,earth,green,weed]
  ,[yellow,blue,blue,lila]
  ,[pink,ash,red,pink]
  ,[earth,mint,blue,lila]
  ,[ash,orange,green,mint]
  ,[],[]
  ]
level919 :: Level
level919 = makeBottle <$>
  [[earth,yellow,earth,weed]
  ,[pink,pink,green,pink]
  ,[mint,weed,yellow,blue]
  ,[green,mint,orange,lila]
  ,[earth,ash,blue,green]
  ,[red,earth,ash,mint]
  ,[weed,orange,orange,lila]
  ,[yellow,yellow,blue,ash]
  ,[red,mint,orange,ash]
  ,[blue,sky,lila,sky]
  ,[green,red,weed,lila]
  ,[sky,sky,red,pink]
  ,[],[]
  ]
level923 :: Level
level923 = makeBottle <$>
  [[ash,lila,mint,yellow]
  ,[weed,ash,blue,mint]
  ,[lila,orange,blue,sky]
  ,[weed,red,sky,green]
  ,[weed,earth,red,blue]
  ,[green,pink,weed,ash]
  ,[orange,earth,earth,lila]
  ,[red,lila,yellow,pink]
  ,[ash,green,blue,yellow]
  ,[earth,sky,yellow,orange]
  ,[green,orange,pink,sky]
  ,[pink,red,mint,mint]
  ,[],[]
  ]
level1013 :: Level
level1013 = makeBottle <$>
  [[lila,lila,weed,mint]
  ,[ash,red,sky,yellow]
  ,[earth,green,lila,earth]
  ,[lila,mint,blue,ash]
  ,[sky,ash,mint,earth]
  ,[pink,yellow,red,sky]
  ,[green,orange,orange,earth]
  ,[green,orange,pink,red]
  ,[orange,pink,weed,red]
  ,[yellow,weed,mint,blue]
  ,[yellow,pink,green,blue]
  ,[ash,blue,weed,sky]
  ,[],[]
  ]
level1051 :: Level
level1051 = makeBottle <$>
  [[lila,yellow,ash,orange]
  ,[earth,sky,ash,mint]
  ,[sky,orange,red,ash]
  ,[earth,lila,blue,earth]
  ,[orange,sky,yellow,lila]
  ,[blue,green,mint,pink]
  ,[sky,green,pink,red]
  ,[weed,orange,yellow,blue]
  ,[weed,earth,ash,mint]
  ,[green,yellow,mint,weed]
  ,[red,pink,blue,red]
  ,[lila,weed,green,pink]
  ,[],[]
  ]

level1297 :: Level
level1297 = makeBottle <$>
  [[orange,red,green,weed]
  ,[ash,yellow,green,lila]
  ,[mint,weed,sky,earth]
  ,[earth,lila,mint,blue]
  ,[lila,red,yellow,earth]
  ,[green,weed,orange,mint]
  ,[mint,ash,sky,orange]
  ,[yellow,sky,pink,weed]
  ,[pink,yellow,green,sky]
  ,[red,orange,blue,pink]
  ,[ash,ash,blue,pink]
  ,[red,lila,blue,earth]
  ,[],[]
  ]

level1337 :: Level
level1337 = makeBottle <$>
  [[mint,blue,sky,lila]
  ,[pink,green,ash,lila]
  ,[weed,orange,ash,earth]
  ,[sky,weed,green,red]
  ,[blue,blue,orange,red]
  ,[earth,yellow,ash,pink]
  ,[weed,sky,mint,lila]
  ,[green,ash,red,pink]
  ,[pink,blue,yellow,orange]
  ,[mint,green,earth,red]
  ,[yellow,sky,orange,mint]
  ,[yellow,lila,earth,weed]
  ,[],[]
  ]

level1767 :: Level
level1767 = makeBottle <$>
  [[orange,yellow,blue,ash]
  ,[orange,pink,ash,green]
  ,[green,yellow,red,mint]
  ,[lila,weed,ash,earth]
  ,[red,orange,sky,sky]
  ,[earth,lila,weed,earth]
  ,[weed,red,weed,green]
  ,[orange,pink,lila,blue]
  ,[lila,blue,yellow,ash]
  ,[green,pink,mint,mint]
  ,[mint,blue,earth,sky]
  ,[sky,red,yellow,pink]
  ,[],[]
  ]

level1815 :: Level
level1815 = makeBottle <$>
  [[yellow,red,yellow,orange]
  ,[orange,sky,mint,green]
  ,[green,red,earth,green]
  ,[mint,yellow,blue,sky]
  ,[blue,earth,orange,ash]
  ,[pink,lila,blue,red]
  ,[mint,pink,earth,lila]
  ,[weed,sky,green,pink]
  ,[lila,weed,earth,ash]
  ,[ash,ash,blue,pink]
  ,[sky,lila,weed,red]
  ,[yellow,weed,orange,mint]
  ,[],[]
  ]

{-

levelXXXX :: Level
levelXXXX = makeBottle <$>
  [[,,,]
  ,[,,,]
  ,[,,,]
  ,[,,,]
  ,[,,,]
  ,[,,,]
  ,[,,,]

  ,[,,,]
  ,[,,,]
  ,[,,,]
  ,[,,,]
  ,[,,,]

  ,[],[]
  ]

-- [ash,blue,earth,green,lila,mint,orange,pink,red,sky,weed,yellow]
-- [ash,blue,earth,green,lila,mint,orange,pink,red,sky,weed,yellow]
-- [ash,blue,earth,green,lila,mint,orange,pink,red,sky,weed,yellow]
-- [ash,blue,earth,green,lila,mint,orange,pink,red,sky,weed,yellow]

-}

--main = solve level141
--main = solve level843
--main = solve level877
--main = solve level919
--main = solve level923
--main = solve level1013
--main = solve level1051
--main = solve level1297
--main = solve level1337
main = solve level1815







--data Prog h a = Pure a
--              | forall b. Bind (Prog h b) (b -> Prog h a)
--              | Spawn [Prog h a]
--              | JoinOn h (Prog h a)
findPathProg :: INVPATH -> Level -> Prog Level PATH
findPathProg !invpath level = do
    let successTASK invpath' = return $ List.reverse invpath'
    guardHistory level
    move <- foreach $ moves level
    let level' = cleanup $ apply move level
    let invpath' = move:invpath
    if isPerfect level'
                then successTASK invpath' -- end of path
                else findPathProg invpath' level'

findPath'Prog :: Level -> IO (Maybe [(From,To,Color,Int)])
findPath'Prog level = do
    let maybePath = listToMaybe $ runProg $ findPathProg [] $ cleanup level
    return $ snd . List.mapAccumL apply' level <$> maybePath


type PATH = [(From,To,Color)]
type INVPATH = PATH

solve :: Level -> IO ()
solve level = do
    path <- showPath <$> findPath'Prog level
    putStrLn $ List.unlines path

showPath :: Maybe [(From,To,Color,Int)] -> [String]
showPath (Just xs) = fmap showMove xs
showPath _ = []

showMove :: (From,To,Color,Int) -> String
showMove (from,to,color,n) = show (from+1) ++ " -> " ++ show (to+1) ++ " (" ++ show n ++ " " ++ show color ++ ")"

--List.mapAccumL apply'
apply' :: Level -> (From,To,Color) -> (Level,(From,To,Color,Int))
apply' level (from,to,color) = (apply ft level,ft')
    where
      level' = cleanup level
      bottleFrom = level' !! from
      Just(n,(==color)->True) = bottleTopLiquid bottleFrom
      bottleTo = level' !! to
      Just from' = List.elemIndex bottleFrom level
      Just to' = List.elemIndex bottleTo level
      to'' = if from' == to'
                    then (1+from'+) $ fromJust $ List.elemIndex bottleTo $ List.drop (from'+1) level
                    else to'
      ft = (from',to'',color)
      ft' = (from',to'',color,n)



type From = Int
type To = Int
moves :: Level -> [(From,To,Color)]
moves level = do
  (bottleTo,j) <- List.zip level [0..]
  guard (not $ bottleIsFull bottleTo)
  (bottleFrom,i) <- List.zip level [0..]
  (amount,color) <- maybeToList $ bottleTopLiquid bottleFrom
  guard (i /= j)
  guard (bottleIsEmpty bottleTo || Just color == bottleTopColor bottleTo)
  do
    -- always move complete color? no, it could be split...
    -- only allow incomplete move if third bottle takes the rest.
    let thirdBottles = do
            (bottle,k)<-List.zip level [0..]
            guard (i /= k)
            guard (j /= k)
            guard (Just color == bottleTopColor bottle)
            guard $ not $ bottleIsFull bottle
            return ()

    guard $ amount <= bottleFreeSpace bottleTo || not(List.null thirdBottles)
    -- no silly move
    guard $ bottleDropN amount bottleFrom /= bottleTo
    -- make no two unicolor bottles of the same color
    guard $ not (bottleIsEmpty bottleTo) || not (List.any (bottleIsUnicolor color) level)
  return (i,j,color)

apply :: (From,To,Color) -> Level -> Level
apply (from,to,_) level =
    [ if i==from then bottleFrom'
      else if i==to then bottleTo'
      else bottle
    | (bottle, i) <- List.zip level [0..]
    ]
  where
    bottleFrom = level !! from
    bottleTo = level !! to
    Just (bottleFrom',bottleTo') = bottleTransferFromTo (bottleFrom,bottleTo)

cleanup :: Level -> Level
cleanup level = List.sort [ bottle
                          | bottle <- level
                          , not (bottleIsComplete bottle)
                          ]

isPerfect :: Level -> Bool
isPerfect = (==) [makeBottle [],makeBottle []]



