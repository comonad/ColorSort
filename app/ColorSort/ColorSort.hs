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



level105 :: Level
level105 = makeBottle <$>
  [[blue,green,sky,sky]
  ,[lila,pink,weed,ash]
  ,[orange,lila,red,earth]
  ,[orange,pink,red,orange]
  ,[green,red,yellow,blue]
  ,[yellow,green,earth,green]
  ,[earth,lila,red,mint]
  ,[mint,lila,pink,mint]
  ,[weed,ash,sky,blue]
  ,[earth,yellow,ash,weed]
  ,[ash,yellow,mint,blue]
  ,[weed,sky,pink,orange]
  ,[],[]
  ]

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

level1827 :: Level
level1827 = makeBottle <$>
  [[yellow,sky,yellow,earth]
  ,[earth,mint,ash,green]
  ,[blue,orange,red,mint]
  ,[lila,lila,red,red]
  ,[blue,yellow,green,mint]
  ,[earth,mint,ash,ash]
  ,[earth,weed,sky,green]
  ,[red,lila,orange,pink]
  ,[yellow,blue,pink,weed]
  ,[orange,green,sky,weed]
  ,[sky,orange,pink,ash]
  ,[pink,blue,lila,weed]
  ,[],[]
  ]

level1901 :: Level
level1901 = makeBottle <$>
  [[lila,blue,yellow,weed]
  ,[yellow,green,yellow,pink]
  ,[yellow,weed,ash,sky]
  ,[sky,earth,red,mint]
  ,[pink,red,blue,ash]
  ,[ash,red,pink,sky]
  ,[mint,orange,green,blue]
  ,[orange,earth,earth,mint]
  ,[sky,lila,weed,lila]
  ,[lila,orange,weed,green]
  ,[ash,red,green,orange]
  ,[blue,earth,pink,mint]
  ,[],[]
  ]

level1987 :: Level
level1987 = makeBottle <$>
  [[blue,yellow,earth,mint]
  ,[orange,earth,orange,pink]
  ,[green,green,yellow,lila]
  ,[earth,orange,red,pink]
  ,[mint,blue,sky,ash]
  ,[weed,lila,weed,lila]
  ,[yellow,green,orange,mint]
  ,[red,pink,red,sky]
  ,[red,blue,ash,green]
  ,[weed,mint,sky,ash]
  ,[weed,lila,blue,earth]
  ,[sky,yellow,ash,pink]
  ,[],[]
  ]

level1989 :: Level
level1989 = makeBottle <$>
  [[lila,weed,sky,red]
  ,[yellow,sky,orange,blue]
  ,[yellow,blue,lila,sky]
  ,[green,ash,sky,red]
  ,[orange,mint,lila,ash]
  ,[red,orange,blue,pink]
  ,[earth,yellow,pink,green]
  ,[orange,yellow,lila,green]
  ,[earth,mint,ash,red]
  ,[mint,weed,pink,green]
  ,[weed,ash,earth,weed]
  ,[earth,mint,blue,pink]
  ,[],[]
  ]

level2041 :: Level
level2041 = makeBottle <$>
  [[ash,earth,red,lila]
  ,[sky,earth,orange,green]
  ,[weed,lila,blue,sky]
  ,[orange,mint,yellow,green]
  ,[green,blue,ash,orange]
  ,[yellow,yellow,green,pink]
  ,[mint,mint,red,lila]
  ,[blue,weed,ash,pink]
  ,[weed,lila,ash,orange]
  ,[blue,red,sky,pink]
  ,[earth,earth,pink,red]
  ,[yellow,weed,mint,sky]
  ,[],[]
  ]

level2101 :: Level
level2101 = makeBottle <$>
  [[orange,weed,blue,green]
  ,[sky,green,ash,pink]
  ,[orange,green,blue,sky]
  ,[ash,yellow,sky,earth]
  ,[earth,red,blue,red]
  ,[mint,red,blue,lila]
  ,[earth,ash,orange,sky]
  ,[pink,pink,lila,mint]
  ,[mint,yellow,orange,red]
  ,[yellow,mint,ash,lila]
  ,[green,weed,weed,pink]
  ,[weed,lila,yellow,earth]
  ,[],[]
  ]

level2113 :: Level
level2113 = makeBottle <$>
  [[yellow,red,blue,yellow]
  ,[weed,lila,ash,weed]
  ,[mint,weed,blue,sky]
  ,[orange,yellow,pink,green]
  ,[lila,sky,green,mint]
  ,[blue,sky,lila,mint]
  ,[lila,yellow,ash,earth]
  ,[earth,ash,pink,sky]
  ,[orange,earth,green,mint]
  ,[pink,earth,orange,red]
  ,[green,red,red,weed]
  ,[pink,orange,ash,blue]
  ,[],[]
  ]

level2273 :: Level
level2273 = makeBottle <$>
  [[pink,yellow,sky,orange]
  ,[lila,weed,ash,yellow]
  ,[green,blue,sky,orange]
  ,[yellow,pink,ash,pink]
  ,[pink,weed,mint,earth]
  ,[earth,blue,ash,red]
  ,[weed,ash,blue,lila]
  ,[orange,green,mint,lila]
  ,[lila,red,mint,mint]
  ,[red,blue,yellow,green]
  ,[red,earth,orange,green]
  ,[sky,earth,sky,weed]
  ,[],[]
  ]


level2289 :: Level
level2289 = makeBottle <$>
  [[green,lila,weed,ash]
  ,[ash,green,mint,mint]
  ,[orange,sky,pink,orange]
  ,[red,lila,ash,blue]
  ,[earth,pink,pink,red]
  ,[yellow,earth,yellow,sky]
  ,[red,weed,blue,sky]
  ,[blue,earth,weed,sky]
  ,[lila,mint,lila,mint]
  ,[ash,weed,earth,green]
  ,[pink,yellow,yellow,red]
  ,[green,orange,blue,orange]
  ,[],[]
  ]

level2421 :: Level
level2421 = makeBottle <$>
  [[earth,orange,pink,orange]
  ,[weed,weed,green,sky]
  ,[mint,ash,green,blue]
  ,[yellow,earth,green,earth]
  ,[ash,ash,sky,mint]
  ,[orange,lila,orange,sky]
  ,[red,earth,pink,lila]

  ,[red,blue,weed,yellow]
  ,[pink,lila,mint,red]
  ,[lila,blue,ash,yellow]
  ,[sky,red,mint,weed]
  ,[blue,pink,green,yellow]

  ,[],[]
  ]

level2561 :: Level
level2561 = makeBottle <$>
  [[sky,earth,earth,sky]
  ,[mint,weed,earth,b]
  ,[blue,sky,yellow,ash]
  ,[red,blue,orange,weed]
  ,[mint,blue,pink,pink]
  ,[weed,sky,orange,f]
  ,[yellow,lila,ash,green]

  ,[mint,lila,yellow,green]
  ,[ash,green,red,mint]
  ,[orange,pink,red,ash]
  ,[lila,earth,lila,orange]
  ,[green,red,yellow,blue]

  ,[],[]
  ]
  where[f,b]=[pink,weed]


level2749 :: Level
level2749 = makeBottle <$>
  [[weed,lila,pink,mint]
  ,[green,red,sky,ash]
  ,[pink,red,lila,weed]
  ,[mint,green,pink,yellow]
  ,[blue,sky,orange,red]
  ,[earth,pink,sky,ash]
  ,[yellow,lila,earth,weed]

  ,[red,green,green,orange]
  ,[blue,mint,yellow,ash]
  ,[weed,lila,sky,earth]
  ,[orange,yellow,orange,earth]
  ,[blue,ash,blue,mint]

  ,[],[]
  ]

level2837 :: Level
level2837 = makeBottle <$>
  [[earth,sky,green,lila]
  ,[red,blue,yellow,earth]
  ,[sky,weed,pink,earth]
  ,[ash,lila,sky,red]
  ,[weed,yellow,orange,green]
  ,[mint,weed,lila,yellow]
  ,[mint,blue,mint,mint]

  ,[pink,pink,red,orange]
  ,[blue,orange,lila,green]
  ,[sky,ash,orange,weed]
  ,[red,ash,green,ash]
  ,[earth,blue,yellow,pink]

  ,[],[]
  ]


level2843 :: Level
level2843 = makeBottle <$>
  [[blue,ash,green,weed]
  ,[blue,earth,lila,mint]
  ,[earth,weed,green,pink]
  ,[sky,lila,earth,ash]
  ,[orange,ash,mint,yellow]
  ,[ash,red,yellow,blue]
  ,[lila,green,orange,pink]

  ,[sky,mint,weed,mint]
  ,[lila,earth,orange,yellow]
  ,[green,orange,red,sky]
  ,[red,red,yellow,pink]
  ,[blue,pink,weed,sky]

  ,[],[]
  ]

level2921 :: Level
level2921 = makeBottle <$>
  [[yellow,yellow,orange,pink]
  ,[red,ash,ash,weed]
  ,[red,mint,mint,green]
  ,[blue,mint,orange,blue]
  ,[red,ash,pink,earth]
  ,[mint,lila,green,lila]
  ,[orange,weed,blue,sky]

  ,[ash,weed,pink,earth]
  ,[yellow,lila,sky,weed]
  ,[blue,sky,orange,green]
  ,[green,yellow,pink,sky]
  ,[earth,earth,lila,red]

  ,[],[]
  ]

level2927 :: Level
level2927 = makeBottle <$>
  [[yellow,lila,weed,orange]
  ,[green,orange,sky,blue]
  ,[ash,lila,sky,earth]
  ,[mint,weed,ash,mint]
  ,[blue,red,red,pink]
  ,[sky,weed,sky,green]
  ,[pink,pink,mint,yellow]

  ,[lila,lila,weed,ash]
  ,[blue,orange,green,yellow]
  ,[earth,earth,red,green]
  ,[pink,earth,ash,red]
  ,[blue,orange,yellow,mint]

  ,[],[]
  ]

level2957 :: Level
level2957 = makeBottle <$>
  [[blue,mint,red,lila]
  ,[lila,earth,green,blue]
  ,[weed,lila,sky,yellow]
  ,[mint,earth,earth,green]
  ,[green,sky,pink,yellow]
  ,[blue,red,ash,earth]
  ,[green,lila,orange,mint]

  ,[ash,sky,yellow,pink]
  ,[weed,orange,pink,ash]
  ,[red,orange,red,ash]
  ,[mint,pink,weed,weed]
  ,[sky,yellow,blue,orange]

  ,[],[]
  ]


level3027 :: Level
level3027 = makeBottle <$>
  [[orange,red,pink,mint]
  ,[sky,ash,orange,earth]
  ,[pink,lila,sky,yellow]
  ,[red,lila,sky,green]
  ,[red,blue,orange,blue]
  ,[earth,pink,mint,weed]
  ,[weed,red,blue,earth]

  ,[yellow,green,earth,yellow]
  ,[ash,orange,ash,green]
  ,[weed,ash,blue,pink]
  ,[sky,lila,yellow,mint]
  ,[mint,green,weed,lila]

  ,[],[]
  ]

level3083 :: Level
level3083 = makeBottle <$>
  [[blue,ash,blue,pink]
  ,[earth,yellow,sky,weed]
  ,[pink,orange,red,orange]
  ,[lila,red,green,weed]
  ,[blue,ash,earth,sky]
  ,[yellow,mint,earth,mint]
  ,[yellow,sky,weed,red]

  ,[mint,orange,yellow,pink]
  ,[ash,green,lila,pink]
  ,[sky,green,lila,red]
  ,[ash,weed,earth,lila]
  ,[mint,orange,blue,green]

  ,[],[]
  ]

level3131 :: Level
level3131 = makeBottle <$>
  [[green,blue,yellow,orange]
  ,[pink,sky,orange,red]
  ,[yellow,green,mint,earth]
  ,[blue,earth,blue,weed]
  ,[sky,ash,mint,ash]
  ,[orange,sky,green,yellow]
  ,[green,weed,sky,mint]

  ,[blue,weed,lila,earth]
  ,[ash,weed,lila,pink]
  ,[orange,red,mint,red]
  ,[lila,pink,earth,pink]
  ,[lila,ash,red,yellow]

  ,[],[]
  ]


-- | impossible ?!
level3145 :: Level
level3145 = makeBottle <$>
  [[yellow,orange,pink,mint]
  ,[mint,ash,weed,green]
  ,[earth,blue,pink,green]
  ,[earth,blue,pink,earth]
  ,[red,sky,orange,green]
  ,[sky,orange,ash,yellow]
  ,[red,green,weed,lila]

  ,[mint,weed,red,yellow]
  ,[yellow,sky,blue,weed]
  ,[orange,sky,ash,earth]
  ,[lila,ash,blue,lila]
  ,[lila,mint,pink,red]

  ,[],[]
  ]


level3213 :: Level
level3213 = makeBottle <$>
  [[pink,weed,yellow,lila]
  ,[earth,blue,lila,lila]
  ,[yellow,yellow,ash,earth]
  ,[pink,green,blue,mint]
  ,[red,sky,mint,earth]
  ,[lila,red,weed,red]
  ,[orange,sky,sky,green]

  ,[orange,green,yellow,orange]
  ,[weed,red,mint,ash]
  ,[blue,ash,ash,pink]
  ,[weed,orange,sky,mint]
  ,[blue,green,pink,earth]

  ,[],[]
  ]


level3319 :: Level
level3319 = makeBottle <$>
  [[ash,ash,earth,lila]
  ,[pink,green,ash,red]
  ,[pink,sky,earth,earth]
  ,[weed,orange,yellow,red]
  ,[blue,orange,blue,yellow]
  ,[red,sky,pink,weed]
  ,[earth,ash,yellow,lila]

  ,[green,pink,blue,mint]
  ,[green,mint,green,sky]
  ,[weed,lila,lila,mint]
  ,[orange,sky,yellow,red]
  ,[weed,orange,mint,blue]

  ,[],[]
  ]

level3683 :: Level
level3683 = makeBottle <$>
  [[earth,earth,orange,lila]
  ,[weed,lila,sky,earth]
  ,[sky,weed,mint,lila]
  ,[sky,pink,green,yellow]
  ,[blue,ash,blue,red]
  ,[green,pink,mint,pink]
  ,[yellow,blue,blue,weed]

  ,[earth,green,green,mint]
  ,[mint,red,orange,lila]
  ,[orange,weed,orange,yellow]
  ,[ash,ash,red,ash]
  ,[yellow,sky,red,pink]

  ,[],[]
  ]

-- | impossible ?!
level3787 :: Level
level3787 = makeBottle <$>
  [[sky,yellow,green,yellow]
  ,[yellow,yellow,ash,ash]
  ,[green,lila,mint,orange]
  ,[sky,red,pink,weed]
  ,[pink,ash,orange,weed]
  ,[lila,orange,earth,mint]
  ,[mint,earth,mint,ash]

  ,[blue,blue,earth,green]
  ,[pink,orange,green,red]
  ,[red,sky,earth,pink]
  ,[weed,red,blue,sky]
  ,[lila,blue,weed,lila]

  ,[],[]
  ]

level3815 :: Level
level3815 = makeBottle <$>
  [[yellow,ash,red,yellow]
  ,[earth,lila,orange,yellow]
  ,[lila,sky,earth,mint]
  ,[mint,blue,red,weed]
  ,[sky,earth,pink,orange]
  ,[mint,orange,pink,red]
  ,[red,earth,pink,mint]

  ,[yellow,ash,green,orange]
  ,[lila,sky,ash,sky]
  ,[blue,blue,green,green]
  ,[weed,weed,ash,blue]
  ,[lila,weed,green,pink]

  ,[],[]
  ]

-- | impossible ?!
level3893 :: Level
level3893 = makeBottle <$>
  [[lila,orange,weed,sky]
  ,[pink,yellow,yellow,weed]
  ,[blue,mint,pink,ash]
  ,[pink,green,red,blue]
  ,[sky,earth,weed,red]
  ,[blue,sky,ash,orange]
  ,[earth,earth,mint,ash]

  ,[green,lila,orange,lila]
  ,[sky,red,yellow,blue]
  ,[yellow,ash,green,mint]
  ,[green,lila,weed,mint]
  ,[pink,red,earth,orange]

  ,[],[]
  ]


main = solve level3893

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


type PATH = [(From,To,Color,Int)]
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
apply' :: Level -> (From,To,Color,Int) -> (Level,(From,To,Color,Int))
apply' level (from,to,color,n) = (apply ft level,ft)
    where
      level' = cleanup level
      bottleFrom = level' !! from
      Just(nn,(==color)->True) = bottleTopLiquid bottleFrom
      bottleTo = level' !! to
      Just from' = List.elemIndex bottleFrom level
      Just to' = List.elemIndex bottleTo level
      to'' = if from' == to'
                    then (1+from'+) $ fromJust $ List.elemIndex bottleTo $ List.drop (from'+1) level
                    else to'
      ft = (from',to'',color,if nn<n then 0 else n)



type From = Int
type To = Int
moves :: Level -> [(From,To,Color,Int)]
moves level = do
  (bottleTo,j) <- List.zip level [0..]
  guard (not $ bottleIsFull bottleTo)
  (bottleFrom,i) <- List.zip level [0..]
  (amount,color) <- maybeToList $ bottleTopLiquid bottleFrom
  guard (i /= j)
  guard (bottleIsEmpty bottleTo || Just color == bottleTopColor bottleTo)
  let amountMoving = (amount `min` bottleFreeSpace bottleTo)
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
    guard $ bottleDropN amountMoving bottleFrom /= bottleTo
    -- make no two unicolor bottles of the same color
    guard $ not (bottleIsEmpty bottleTo) || not (List.any (bottleIsUnicolor color) level)
    -- do not join unicolor bottles 3 atop 1
    when (amount==3) $ do
        guard . not $ (bottleFillLevel bottleFrom == 3) && (bottleFillLevel bottleTo == 1)
  return (i,j,color,amountMoving)

apply :: (From,To,Color,Int) -> Level -> Level
apply (from,to,_,_) level =
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



