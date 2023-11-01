module Main where

import Data.List as List
import Control.Monad as Monad
import Data.Maybe (listToMaybe,fromJust)

[ash,blue,earth,green,lila,mint,orange,pink,red,sky,weed,yellow]="abeglmoprswy"
bottleMaxHeight = 4 :: Int

showColor :: Color -> String
showColor c = List.head [ w
                        | (w@(c1:_))<-List.words "ash blue earth green lila mint orange pink red sky weed yellow"
                        , c == c1]


type Color = Char
type Level = [[Color]]
level141 :: Level
level141 =
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
level843 =
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
level877 =
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
level919 =
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
level923 =
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
level1013 =
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


main = solve level1013

findPath :: [Level] -> Level -> Maybe [(From,To,Color)]
findPath history level = listToMaybe $ do
    guard $ List.notElem level history
    move <- moves level
    let level' = cleanup $ apply move level
    (Just cont) <- [if isPerfect level'
            then Just [] -- end of path
            else findPath (level:history) level']
    return (move:cont)

findPath' :: Level -> Maybe [(From,To,Color,Int)]
findPath' level = do
    path <- findPath [] $ cleanup level
    return $ snd $ List.mapAccumL apply' level path

solve :: Level -> IO ()
solve level = do
    let path = showPath $ findPath' level
    putStrLn $ List.unlines path

showPath :: Maybe [(From,To,Color,Int)] -> [String]
showPath (Just xs) = fmap showMove xs
showPath _ = []

showMove :: (From,To,Color,Int) -> String
showMove (from,to,color,n) = show (from+1) ++ " -> " ++ show (to+1) ++ " (" ++ show n ++ " " ++ showColor color ++ ")"

--List.mapAccumL apply'
apply' :: Level -> (From,To,Color) -> (Level,(From,To,Color,Int))
apply' level (from,to,color) = (apply ft level,ft')
    where
      level' = cleanup level
      bottleFrom = level' !! from
      n = List.length $ List.takeWhile(==color) bottleFrom
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
  guard (List.length bottleTo < 4)
  (bottleFrom@(color:_),i) <- List.zip level [0..]
  guard (i /= j)
  guard (List.null bottleTo || color == List.head bottleTo)
  do
    -- always move complete color
    let amount = List.length $ List.takeWhile (==color ) bottleFrom
    guard $ amount + List.length bottleTo <= bottleMaxHeight
    -- no silly move
    guard $ List.drop amount bottleFrom /= bottleTo
    -- make no two unicolor bottles of the same color
    guard $ not (List.null bottleTo) || List.null [()| bottle@(_:_) <- level, List.all (==color) bottle]
  return (i,j,color)

apply :: (From,To,Color) -> Level -> Level
apply (from,to,_) level =
    [ if i==from then bottleFrom'
      else if i==to then bottleTo'
      else bottle
    | (bottle, i) <- List.zip level [0..]
    ]
  where
    bottleFrom@(color:_) = level !! from
    bottleTo = level !! to
    transfer = takeWhile (==color) . take (bottleMaxHeight - length bottleTo) $ bottleFrom
    bottleFrom' = List.drop (length transfer) bottleFrom
    bottleTo' = transfer <> bottleTo

cleanup :: Level -> Level
cleanup level = List.sort [ bottle
                          | bottle <- level
                          , List.length bottle < 4 || List.any (/= head bottle) bottle
                          ]

isPerfect :: Level -> Bool
isPerfect = List.null . mconcat



