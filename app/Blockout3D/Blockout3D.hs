{-# LANGUAGE ViewPatterns, TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}


module Main where

import Data.List as List
import Control.Monad as Monad
import Control.Monad.Trans.Maybe as Maybe
import Data.Maybe (listToMaybe,fromJust,catMaybes,maybeToList)
import Control.Concurrent

import Data.IORef
import Data.Function
import Data.Set as Set
import Data.Map.Strict as Map
import Data.IntMap.Strict as IntMap
import Data.Foldable as Foldable

import FreeMonoid

import Prog




data XY = XY !Int !Int
  deriving (Eq,Ord)
instance Show XY where
  show (XY x y) = "<"<>show x<>"|"<>show y<>">"

instance Monoid XY where
  mempty = XY 0 0
instance Semigroup XY where
  (<>) (XY a b)(XY c d) = XY (a+c) (b+d)
  
type Color = Char
data Field = Floor | Hole !Color
data Piece = Piece !Color !(Set XY)
  deriving (Eq,Ord)
  deriving Show
  
type PieceId = Int
data PlacedPiece = PlacedPiece !PieceId !(Maybe PlacedPiece)
  deriving (Eq,Ord)
  deriving Show


data Level = Level
  { walls :: !(Set XY)
  , holes :: !(Map Color (Set XY))
  , pieces :: !(IntMap Piece)
  , placed :: !(Map XY PlacedPiece) -- = HistoryEvent
  , covered :: !(Set XY)
  }
  deriving Show
type HistoryEvent = Map XY PlacedPiece

data Direction = N|S|W|E
  deriving (Eq,Ord,Enum,Bounded)
instance Show Direction where
  show N = "↑"
  show S = "↓"
  show W = "←"
  show E = "→"
type Move = (XY,Direction)
applyDir :: Direction -> XY -> XY
applyDir N (XY x y) = XY x (y-1)
applyDir S (XY x y) = XY x (y+1)
applyDir W (XY x y) = XY (x-1) y
applyDir E (XY x y) = XY (x+1) y

level159 :: Level
level159 = makeLevel
  [ "xxxxxxxxx"
  , "x  aaa  x"
  , "xx  a  xx"
  , "xx      x"
  , "x       x"
  , "x       x"
  , "x     b x"
  , "xccc xbcx"
  , "x cx xccx"
  , "xxxxxxxxx"
  ]
  [ "xxxxxxxxx"
  , "x11   55x"
  , "xx12 35xx"
  , "xx22a34 x"
  , "xbbbacccx"
  , "xjb666cdx"
  , "xjAB6e7dx"
  , "xiiBhxfgx"
  , "xkixhxfgx"
  , "xxxxxxxxx"
  ]
  [("1234567","c")
  ,("B","b")
  ,("abcdefghijk","a")
  ,("A","ac")
  ]


makeLevel :: [String] -> [String] -> [(String,String)] -> Level
makeLevel arena situation colorstacks = Level{..}
  where
    fields :: Map XY Field
    fields = Map.fromList do
      (y,xs) <- zip [0..] arena
      (x,a)  <- zip [0..] xs
      guard $ a /= 'x'
      return (XY x y , if a==' ' then Floor else Hole a)

    unify :: [XY] -> (XY,Set XY)
    unify xy = let xy0@(XY x y) = minimum xy
                in (xy0,Set.fromList $ (XY (-x)(-y)<>) <$> xy)

    parts :: Map Char [XY]
    parts = Map.fromListWith(<>) do
      (y,xs) <- zip [0..] situation
      (x,a)  <- zip [0..] xs
      guard $ a /= 'x'
      guard $ a /= ' '
      return (a,[XY x y])
      
    colormap :: Map Char [Color]
    colormap = Map.fromList do
      (as,cs) <- colorstacks
      a <- as
      return (a,cs)

    pcs :: [(XY,[Piece])]  
    pcs = do
      ((a,unify->(xy,sxy)),(a',cs)) <- Map.toAscList parts `zip` Map.toAscList colormap
      if a/=a' then error $ "makeLevel item mismatch " <> show (a,a') else return ()
      return (xy,[Piece c sxy|c<-cs])

    pieces_set :: Set Piece
    pieces_set = Set.fromList $ pcs >>= snd
    pieces' :: Map Piece PieceId
    pieces' = Map.fromAscList $ Set.toAscList pieces_set `zip` [0..]
    pieces :: IntMap Piece
    pieces = IntMap.fromList $ swap <$> Map.toList pieces'
    swap (a,b)=(b,a)
    
    mkp :: [Piece] -> Maybe PlacedPiece
    mkp [] = Nothing
    mkp (p:ps) = Just $ PlacedPiece (pieces' Map.! p) (mkp ps)
    mkpp :: XY -> [Piece] -> (XY,PlacedPiece)
    mkpp xy (mkp -> Just pp) = (xy,pp)
    mkpp _ _ = error "mkpp"
    
    placed :: Map XY PlacedPiece
    placed = Map.fromList $ uncurry mkpp <$> pcs
    
      -- fields :: Map XY Field
    walls :: Set XY
    walls = let fs = Map.keysSet fields
             in Set.unions [ Set.mapMonotonic (XY (-1) (0)<>) fs
                           , Set.mapMonotonic (XY (0) (-1)<>) fs
                           , Set.mapMonotonic (XY (1) (0)<>) fs
                           , Set.mapMonotonic (XY (0) (1)<>) fs
                           ] Set.\\ fs
    
    holes :: Map Color (Set XY)
    holes = Map.fromListWith(<>) do
      (xy,Hole c) <- Map.toList fields
      return (c,Set.singleton xy)

    covered :: Set XY
    covered = Set.unions $ walls : do
      (yx0,PlacedPiece i _) <- Map.toList placed
      let (Piece _ sxy) = pieces IntMap.! i
      return $ Set.mapMonotonic (yx0<>) sxy
    


main = solve level159

showPath :: [Move] -> [String]
showPath [] = []
showPath ((xy,d):ms) = sp (show xy <> " : " <> show d) (applyDir d xy) ms
  where
    sp s _ [] = [s]
    sp s xye ((xy,d):ms) | xye == xy = sp (s <> show d) (applyDir d xy) ms
    sp s _ ms = s : showPath ms



solve :: Level -> IO ()
solve level = do
    let showPath' :: Maybe ([Move],Level) -> [String]
        showPath' = maybe [] (\(m,l)->showPath m <> ["falls_into_hole"])
    mpl <- findPath'Prog level
    putStrLn $ List.unlines $ showPath' mpl
    case mpl of
      Nothing | isPerfect level -> putStrLn "LEVEL CLEARED"
              | otherwise -> putStrLn "PROBLEM FOUND IN LEVEL" >> print level
      Just (_,l) -> solve l

findPath'Prog :: Level -> IO (Maybe ([Move],Level))
findPath'Prog level = do
    let maybePath = listToMaybe $ runProg $ findPathProg [] level
    return $ maybePath


--data Prog h a = Pure a
--              | forall b. Bind (Prog h b) (b -> Prog h a)
--              | Spawn [Prog h a]
--              | JoinOn h (Prog h a)
type PATH = [Move]
type INVPATH = [Move]
findPathProg :: INVPATH -> Level -> Prog HistoryEvent (PATH,Level)
findPathProg !invpath level = do
    let successTASK invpath' = return $ List.reverse invpath'
    guardHistory (placed level)
    (falls_into_hole,move,level') <- possibleMoves level
    let invpath' = move:invpath
    --if isPerfect level'
    if falls_into_hole
                then (,level') <$> successTASK invpath' -- end of path
                else findPathProg invpath' level'

isPerfect :: Level -> Bool
isPerfect = Map.null . placed

possibleMoves :: Level -> Prog historyEvent (Bool,Move,Level)
possibleMoves Level{..} = do
  (xy,pp@(PlacedPiece i roof)) <- foreach $ Map.toAscList placed
  let p@(Piece color (pset0::Set XY)) = pieces IntMap.! i
  let psetA = Set.mapMonotonic (xy<>) pset0
  let covered_ :: Set XY
      covered_ = covered Set.\\ psetA
  (direction,delta) <- foreach $ [N,S,W,E]`zip`[XY 0 (-1),XY 0 1,XY (-1) 0,XY 1 0]
  let psetB = Set.mapMonotonic (delta<>) psetA
  guard $ Set.disjoint psetB covered_
  let falls_into_hole = psetB `Set.isSubsetOf` (holes Map.! color)
  let pp' = case (falls_into_hole,roof) of
                  (False,_) -> Just pp
                  (True,Nothing) -> Nothing
                  (True,Just pp) -> Just pp
  let level' = case pp' of
                  Nothing -> Level
                      { covered = covered_
                      , placed = Map.delete xy $ placed
                      , ..
                      }
                  (Just pp) -> Level
                      { covered = covered_ `Set.union` psetB
                      , placed = Map.insert (xy<>delta) pp $ Map.delete xy $ placed
                      , ..
                      }
  return (falls_into_hole,(xy,direction),level')

{-

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


-}
