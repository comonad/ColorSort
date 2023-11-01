module Main where

import Data.List as List
import Control.Monad as Monad
import Control.Monad.Trans.Maybe as Maybe
import Data.Maybe (listToMaybe,fromJust,catMaybes)
import Control.Concurrent

import Data.IORef
import Data.Function
import Data.Set as Set

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

--main = solve level141
--main = solve level843
--main = solve level877
--main = solve level919
--main = solve level923
main = solve level1013



type HISTORY = IORef (Set Level)
type TASK = IO (Maybe [(From,To,Color)])
type TODOS = IORef (Maybe[TASK])

type X a = MaybeT IO a
guardHistory :: HISTORY -> Level -> X ()
guardHistory history level = MaybeT $ do
    atomicModifyIORef' history (\s -> (Set.insert level s, guard $ Set.notMember level s))
createHistory :: IO HISTORY
createHistory = newIORef Set.empty

insertTODOs :: TODOS -> [TASK] -> IO ()
insertTODOs todos tasks = do
        atomicModifyIORef' todos (\mt -> (fmap (tasks<>) mt, ()))
createTODOS :: IO TODOS
createTODOS = newIORef $ Just []
takeTODO :: TODOS -> Int -> IO (Maybe [TASK])
takeTODO todos n = do
    atomicModifyIORef' todos (\mt -> (fmap (List.drop n) mt, fmap (List.take n) mt))
terminateAllTODOs :: TODOS -> IO ()
terminateAllTODOs todos = atomicModifyIORef' todos (\mt -> (Nothing, ()))

runTODOs :: TODOS -> IO (Maybe [(From,To,Color)])
runTODOs todos = do
    result <- newEmptyMVar
    forkIO $ fix $ \loop -> do
        t <- takeTODO todos 16 :: IO (Maybe [TASK])
        case t of
            Nothing -> return ()
            Just [] -> do
                threadDelay 1000000000
                -- all tasks ended and no todos? putMVar result Nothing
                loop
            Just ts -> do
                rs <- sequence ts
                case catMaybes rs of
                    (r:_) -> putMVar result (Just r)
                    _ -> loop

    takeMVar result

type PATH = [(From,To,Color)]
type INVPATH = PATH
successTASK :: TODOS -> INVPATH -> TASK
successTASK todos invpath' = do
    terminateAllTODOs todos
    return $ Just $ List.reverse invpath'

forkTASKs :: TODOS -> [TASK] -> TASK
forkTASKs todos tasks = do
    insertTODOs todos tasks
    return Nothing


findPath :: HISTORY -> TODOS -> INVPATH -> Level -> TASK
findPath history todos !invpath level = runMaybeT $ do
    guardHistory history level
    MaybeT $ forkTASKs todos $ do
        move <- moves level
        let level' = cleanup $ apply move level
        let invpath' = move:invpath
        return $ if isPerfect level'
                    then successTASK todos invpath' -- end of path
                    else findPath history todos invpath' level'


findPath' :: Level -> IO (Maybe [(From,To,Color,Int)])
findPath' level = do
    history <- createHistory
    todos <- createTODOS
    insertTODOs todos [findPath history todos [] $ cleanup level]
    maybePath <- runTODOs todos
    return $ snd . List.mapAccumL apply' level <$> maybePath

solve :: Level -> IO ()
solve level = do
    path <- showPath <$> findPath' level
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
    transfer = takeWhile (==color) . List.take (bottleMaxHeight - length bottleTo) $ bottleFrom
    bottleFrom' = List.drop (length transfer) bottleFrom
    bottleTo' = transfer <> bottleTo

cleanup :: Level -> Level
cleanup level = List.sort [ bottle
                          | bottle <- level
                          , List.length bottle < 4 || List.any (/= head bottle) bottle
                          ]

isPerfect :: Level -> Bool
isPerfect = List.null . mconcat



