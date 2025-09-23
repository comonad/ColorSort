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

import Data.Char as Char
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
  
minus :: XY -> XY -> XY
minus (XY a b)(XY c d) = XY (a-c) (b-d)
  
type Color = Char

data Piece = Piece !Color !(Set XY) ![PieceGoal]
  --deriving (Eq,Ord)
  deriving Show
data Gate = Gate ![Color] !(Set XY) !Direction !Int ![Int] 
  deriving (Eq,Ord)
  deriving Show
  
type PieceId = Int
type GateId = Int

data PieceGoal = PieceGoal
  { goal_pos :: !XY
  , goal_direction :: !Direction
  , goal_condition_emptyfields :: !(Set XY)
  , goal_modulus :: !Int
  , goal_condition_reminders :: ![Int]
  }
  deriving Show
data Level = Level
  { walls :: !(Set XY)
  , pieces :: !(IntMap Piece)
  , piecepositions :: !(IntMap XY)
  , finished :: !Int
  , covered :: !(Set XY) -- includes walls, goals and pieces
  }
  deriving Show
data HistoryEvent = HistoryEvent { he_finished:: !Int,he_piecepositions :: !(IntMap XY) }
  deriving (Eq,Ord)
  deriving Show
mkHistoryEvent :: Level -> HistoryEvent
mkHistoryEvent l = HistoryEvent (finished l) (piecepositions l)

data Direction = N|S|W|E
  deriving (Eq,Ord,Enum,Bounded)
instance Show Direction where
  show N = "↑"
  show S = "↓"
  show W = "←"
  show E = "→"
data Move = Move Int Color XY Direction Bool
applyDir :: Direction -> XY -> XY
applyDir N (XY x y) = XY x (y-1)
applyDir S (XY x y) = XY x (y+1)
applyDir W (XY x y) = XY (x-1) y
applyDir E (XY x y) = XY (x+1) y



level671 :: Level
level671 = makeLevel
  [ "####L###"
  , "##ggoo##"
  , "O ygor #"
  , "Obl#lygS"
  , "P lbpy S"
  , "Prrbpss#"
  , "##rsos##"
  , "#RyyppB#"
  , "#Ry  pB#"
  , "##GGYY##"
  ]
  [ "####L###"
  , "##0101##"
  , "O 0110 #"
  , "O00#222S"
  , "P 1203 S"
  , "P233101#"
  , "##3221##"
  , "#R4523B#"
  , "#R5  3B#"
  , "##GGYY##"
  ]
  [ (N,"Ll",1,[0]) -- Dir,gatecolor:piececolors,n,ks -> (steps mod n `elem` ks)
  , (E,"Ss",2,[0])
  , (E,"Bb",2,[1])
  , (S,"Yy",1,[0])
  , (S,"Gg",1,[0])
  , (W,"Rr",1,[0])
  , (W,"Pp",1,[0])
  , (W,"Oo",1,[0])
  ]

makeLevel :: [String] -> [String] -> [(Direction,String,Int,[Int])] -> Level
makeLevel arena pieceparts gates_raw = Level{..}
  where
    fields :: Map XY (Char,Char)
    fields = Map.fromList [ (XY x y,(c,p))
                          | (y,row,parts) <- List.zip3 [0..] arena pieceparts
                          , (x,c,p) <- List.zip3 [0..] row parts
                          ]
    walls :: Set XY
    walls = Set.fromList [ xy
                         | (xy,(c,p)) <- Map.toList fields
                         ,  c=='#' || Char.isUpper c
                         ]
    
    gatepositions :: Map Char [XY]
    gatepositions = Map.fromListWith(<>)
                        [ (c,[xy])
                        | (xy,(c,_)) <- Map.toList fields
                        , Char.isUpper c
                        ]
    gates :: [Gate]
    gates = [ Gate cs gps d m rs
            | (d,c:cs,m,rs) <- gates_raw
            , let gps = Set.fromList $ gatepositions Map.! c :: Set XY
            ]
    reverse_fields :: Map (Char,Char)[XY]
    reverse_fields = Map.fromListWith(<>)[(cp,[xy])|(xy,cp)<-Map.toList fields]

    ppp :: [(Int,(Piece,XY))]
    ppp = List.zip [0..] do
      (xy,(c,p)) <- Map.toList fields
      guard $ isLower c && Char.isNumber p && (read[p]`mod`2==(0::Int))
      let xyr :: [XY]
          xyr = maybe [] id $ (c,succ p) `Map.lookup` reverse_fields
      let shape :: [XY]
          shape = [p`minus`xy|p<-xy:xyr]
      let t = minimum [y | (XY x y) <- shape]
          b = maximum [y | (XY x y) <- shape]
          l = minimum [x | (XY x y) <- shape]
          r = maximum [x | (XY x y) <- shape]
      
      let piecegoals :: [PieceGoal]
          piecegoals = do
            (Gate cs xys goal_direction goal_modulus goal_condition_reminders
              ) <- gates :: [Gate]
            guard $ c `List.elem` cs
            let (Just z) = Set.lookupMin xys
                gatesize = Set.size xys
            let piecesize = case goal_direction of
                  N -> (r-l+1)
                  S -> (r-l+1)
                  W -> (b-t+1)
                  E -> (b-t+1)
            guard $ piecesize <= gatesize
            let gs = do
                  step <- [0..gatesize-piecesize]
                  return $ case goal_direction of
                    N -> z <> XY (step-l) (1-t)
                    S -> z <> XY (step-l) (-1-b)
                    W -> z <> XY (1-l) (step-t)
                    E -> z <> XY (-1-r) (step-t)
            goal_pos <- gs
            let gcef = Set.fromList do
                  p <- shape
                  let (XY px py) = goal_pos<>p
                      (XY gx gy) = z
                  case goal_direction of
                    N -> [XY px y|y<-[gy+1..py-1]]
                    S -> [XY px y|y<-[py+1..gy-1]]
                    W -> [XY x py|x<-[gx+1..px-1]]
                    E -> [XY x py|x<-[px+1..gx-1]]
            let goal_condition_emptyfields :: Set XY
                goal_condition_emptyfields = gcef Set.\\ Set.fromList shape
            return PieceGoal{..}
      return (Piece c (Set.fromList shape) piecegoals,xy)
    
  
    --ppp :: [(Piece,XY,[PieceGoal])]
    pieces :: IntMap Piece
    pieces =
      IntMap.fromList $ [ (i,p) | (i,(p,_)) <- ppp ]
    piecepositions :: IntMap XY
    piecepositions =
      IntMap.fromList $ [ (i,p) | (i,(_,p)) <- ppp ]

    finished = 0 :: Int
    covered :: Set XY
    covered = Set.fromList do
      (xy,(c,_)) <- Map.toList fields
      guard $ c /= ' '
      return xy







{-
data PlacedPiece = PlacedPiece !PieceId !(Maybe PlacedPiece)
  deriving (Eq,Ord)
  deriving Show
type HistoryEvent = Map XY PlacedPiece

-}

main = solve level671

showPath :: [Move] -> [String]
showPath [] = []
showPath (Move i c xy d True :ms) = (show (c,xy) <> " : " <> show d <> " *") : showPath ms
showPath (Move i c xy d False:ms) = sp (show (c,xy) <> " : " <> show d) ms
  where
    sp s [] = [s]
    sp s (Move j c xy d True :ms) | i==j = (s <> show d <> " *")  : showPath ms
    sp s (Move j c xy d False:ms) | i==j = sp (s <> show d) ms
    sp s ms = s : showPath ms



solve :: Level -> IO ()
solve level = do
    let showPath' :: Maybe ([Move],Level) -> [String]
        showPath' = maybe [] (\(m,l)->showPath m <> ["SOLVED"])
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
    let successTASK invpath' level' = return (List.reverse invpath',level')
    guardHistory (mkHistoryEvent level)
    (scores_a_goal,move,level') <- possibleMoves level
    let invpath' = move:invpath
    if isPerfect level'
      then successTASK invpath' level' -- end of path
      else do
        when scores_a_goal do
          guard $ isSolvable level'
          raisePrio 1
        findPathProg invpath' level'

isPerfect :: Level -> Bool
isPerfect Level{..} = and do
  -- all pieces that are left on board have no gates
  i <- IntMap.keys piecepositions
  let (Piece color (pset0::Set XY) piecegoals) = pieces IntMap.!i
  return $ List.null piecegoals
isSolvable :: Level -> Bool
isSolvable Level{..} = or do
  -- assuming it is not perfect yet
  -- any piece that is left on board can goal
  i <- IntMap.keys piecepositions
  let (Piece color (pset0::Set XY) piecegoals) = pieces IntMap.!i
  PieceGoal{..} <- piecegoals
  return $ (finished `mod` goal_modulus) `List.elem` goal_condition_reminders
  


validPosition :: Piece -> XY -> Set XY -> Bool
validPosition (Piece _color (pset0::Set XY) _piecegoals) xy covered =
  covered `Set.disjoint` Set.map (xy <>) pset0

scoreGoal :: Piece -> XY -> Int -> Set XY -> Maybe Direction
scoreGoal (Piece _color (_pset0::Set XY) piecegoals) xy finished covered =
  listToMaybe do
    PieceGoal{..} <- piecegoals
    guard $ xy == goal_pos
    guard $ (finished `mod` goal_modulus) `List.elem` goal_condition_reminders
    guard $ covered `Set.disjoint` goal_condition_emptyfields
    return goal_direction

possibleMoves :: Level -> Prog historyEvent (Bool,Move,Level)
possibleMoves Level{..} = do
  (i,xy) <- foreach $ IntMap.toAscList piecepositions
  let p@(Piece color (pset0::Set XY) piecegoals) = pieces IntMap.! i
      covered_without_p = covered Set.\\ Set.map (xy<>) pset0
  let move d goal = Move i color xy d goal
  case scoreGoal p xy finished covered_without_p of
    Just d -> return (True, move d True,
      Level { piecepositions = IntMap.delete i piecepositions
            , finished = 1+finished
            , covered = covered_without_p
            , ..
            }
      )
    Nothing -> do
      d <- foreach [minBound..maxBound::Direction]
      let xy' = applyDir d xy
      guard $ validPosition p xy' covered_without_p
      case scoreGoal p xy' finished covered_without_p of
        Just d -> return (True, move d True,
          Level { piecepositions = IntMap.delete i piecepositions
                , finished = 1+finished
                , covered = covered_without_p
                , ..
                }
          )
        Nothing -> return (False, move d False,
          Level { piecepositions = IntMap.insert i xy' piecepositions
                , covered = covered_without_p `Set.union` Set.map (xy' <>) pset0
                , ..
                }
          )




      



