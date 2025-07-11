{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeOperators #-}


-- | Nine Men's Morris
module Main where

import Data.Map.Strict as Map
import Data.Set as Set
import Data.IntSet as IntSet
import Data.IntMap.Strict as IntMap
import Data.List as List
import Data.Function
import Data.Ratio
import Control.Monad as Monad
import Data.Bits
import qualified Data.Vector as Vector
import Data.Vector (Vector)

newtype Board = Board_ Int
    deriving (Eq,Ord)
emptyBoard :: Board
emptyBoard = Board_ 0
lookupBoard :: Board -> Coords -> Maybe Player
lookupBoard (Board_ board) (fromEnum->c) =
    case (board `shiftR` (2*c)).&. 3 of
        0 -> Nothing
        1 -> Just Black
        2 -> Just White
        x -> error $ "pattern match in lookupBoard: " <>show x
setBoard :: Coords -> Maybe Player -> Board -> Board
setBoard (fromEnum->c) x (Board_ board) = Board_ $ board .&. complement(3 `shiftL` (2*c)) .|. case x of
    Nothing -> 0
    Just Black -> 1 `shiftL` (2*c)
    Just White -> 2 `shiftL` (2*c)


boardSymmetry :: Board -> IntSet
boardSymmetry b = IntSet.fromList $ do
    y <- [mirrorBoardY b,b]
    x <- [mirrorBoardX y,y]
    z <- [mirrorBoardZ x,x]
    (Board_ r) <- [rotateBoard z,z]
    pure r

rotateBoard :: Board -> Board
rotateBoard (Board_ board) = Board_ $ (cw1 .&. mask) .|. (ccw3 .&. complement mask)
    where
        cw1 = board `shiftL` 4
        ccw3 = board `shiftR` 12
        mask = 0xfff0fff0fff0
mirrorBoardZ :: Board -> Board
mirrorBoardZ (Board_ board) = Board_ $ a .|. b .|. c
    where
        a = (board .&. 0xffff) `shiftL` 32
        b = board `shiftR` 32
        c = board .&. 0xffff0000
mirrorBoardX :: Board -> Board
mirrorBoardX (Board_ board) = Board_ $ cw0 .|. cw1 .|. cw2 .|. ccw1 .|. ccw2
    where
        cw0 = (board .&. 0x0c0c0c0c0c0c)
        cw1 = (board .&. 0x030303030303) `shiftL` 4
        cw2 = (board .&. 0x00c000c000c0) `shiftL` 8
        ccw1 = (board .&. 0x303030303030) `shiftR` 4
        ccw2 = (board .&. 0xc000c000c000) `shiftR` 8
mirrorBoardY :: Board -> Board
mirrorBoardY (Board_ board) = Board_ $ cw0 .|. cw1 .|. cw2 .|. cw3 .|. ccw1 .|. ccw2 .|. ccw3
    where
        cw0 = (board .&. 0xc0c0c0c0c0c0)
        cw1 = (board .&. 0x003000300030) `shiftL` 4
        cw2 = (board .&. 0x000c000c000c) `shiftL` 8
        cw3 = (board .&. 0x000300030003) `shiftL` 12
        ccw1 = (board .&. 0x030003000300) `shiftR` 4
        ccw2 = (board .&. 0x0c000c000c00) `shiftR` 8
        ccw3 = (board .&. 0x300030003000) `shiftR` 12



playerFields :: Board -> Player -> [Coords]
playerFields board player = [c|c<-[minBound..maxBound],lookupBoard board c == Just player]
emptyFields :: Board -> [Coords]
emptyFields board = [c|c<-[minBound..maxBound],lookupBoard board c == Nothing]


isMill :: Board -> Coords -> Bool
isMill board c = not . List.null $ do
    let x = lookupBoard board c
    m <- possibleMills c
    let cs = Set.toList $ Set.delete c m
    guard $ List.all ((x==) . lookupBoard board) cs

possibleMills :: Coords -> [Set Coords]
possibleMills = (Map.!) $ Map.fromListWith (<>) $ mconcat[[(c,[s])]|s<-allMills,c<-Set.toList s]
allMills :: [Set Coords]
allMills = star <> rings 
    where
    rings = do
        s <- [minBound..maxBound::Stage]
        let cps = cycle [makeCoords s c p
                        |c<-[minBound..maxBound::Compass]
                        ,p<-[minBound..maxBound::Position]
                        ]
        List.take 4 $ mconcat $ List.zipWith ($) (cycle [return . Set.fromList . List.take 3,const[]]) (List.tails cps)
    star = do
        c<-[minBound..maxBound::Compass]
        return $ Set.fromList[makeCoords s c Center|s<-[minBound..maxBound::Stage]]




data Stage = StageOuter|StageMid|StageInner
    deriving (Eq,Ord,Enum,Bounded,Show)
data Compass = N|E|S|W
    deriving (Eq,Ord,Enum,Bounded,Show)
data Position = Corner|Center
    deriving (Eq,Ord,Enum,Bounded,Show)

compassCCW :: Compass -> Compass
compassCCW = toEnum  . (\i->(i+3) `mod` 4) . fromEnum
compassCW :: Compass -> Compass
compassCW = toEnum  . (\i->(i+1) `mod` 4) . fromEnum

--data Coords = Coords Stage Compass Position
--    deriving (Eq,Ord,Bounded)
newtype Coords = Coords Int
    deriving (Eq,Ord,Enum)
makeCoords :: Stage -> Compass -> Position -> Coords
makeCoords (fromEnum->s) (fromEnum->c) (fromEnum->p) = toEnum $ s*8 + c*2 + p
matchCoords :: Coords -> (Stage,Compass,Position)
matchCoords (fromEnum->coords) =
        let (s,cp) = coords `divMod` 8
            (c,p) = cp `divMod` 2
        in (toEnum s::Stage,toEnum c::Compass,toEnum p::Position)

instance Bounded Coords where
    minBound = Coords 0
    maxBound = Coords 23
instance Show Coords where
    show = show . matchCoords

--instance Enum Coords where
--    fromEnum (Coords s c p) = fromEnum s*8 + fromEnum c*2 + fromEnum p
--    toEnum i = let (s,cp) = i `divMod` 8
--                   (c,p) = cp `divMod` 2
--                in Coords (toEnum s) (toEnum c) (toEnum p)

instance Read Coords where
    --readsPrec :: Int -> String -> [(Coords, String)]
    readsPrec _ st = do
        let mkCoords s x = makeCoords s c p  
                where
                    Just c = List.lookup x $ "78963214" `List.zip` [N,N,E,E,S,S,W,W]
                    Just p = List.lookup x $ "78963214" `List.zip` cycle[Corner,Center]
        case st of
            [] -> []
            (i:s) | i `notElem` "78946123" -> []
            (i:j:k:s) | i==j && i==k -> [(mkCoords StageInner i,s)]
            (i:j:s) | i==j -> [(mkCoords StageMid i,s)]
            (i:s) -> [(mkCoords StageOuter i,s)]


data Player = Black|White
    deriving (Eq,Ord,Enum,Bounded)

opponent :: Player -> Player
opponent p = toEnum $ 1-fromEnum p


newtype PlayerCounts = PlayerCounts_ Int
const' :: Int -> PlayerCounts
const' b | b>=0 && b<16 = PlayerCounts_ ((b `shiftL` 4) .|. b)
const' b = error $ "pattern match in const': " <> show b

apply :: PlayerCounts -> Player -> Int
apply (PlayerCounts_ i) Black = i .&. 0x0f
apply (PlayerCounts_ i) White = i `shiftR` 4
modPlayer :: Player -> (Int->Int) -> PlayerCounts -> PlayerCounts
modPlayer Black f (PlayerCounts_ i) = PlayerCounts_ $ (i .&. 0x0f0) .|. f (i.&.0x0f)
modPlayer White f (PlayerCounts_ i) = PlayerCounts_ $ (i .&. 0x00f) .|. (f (i `shiftR` 4)`shiftL` 4)


--data a :-> b = Mapping[(a,b)]
--const' :: (Enum a,Bounded a) => b -> (a :-> b)
--const' b = Mapping $ [minBound..maxBound]`zip`cycle[b]
--apply :: Eq a => (a :-> b) -> a -> b
--apply (Mapping m) a = List.head [b|(a',b)<-m,a'==a] 
--modPlayer :: Player -> (Int->Int) -> (Player :-> Int) -> (Player :-> Int)
--modPlayer p f (Mapping m) = Mapping [(a,if a==p then f b else b)|(a,b)<-m]


data Game = Game
    { board :: Board
    , unplayed :: PlayerCounts
    , captured :: PlayerCounts
    , playersTurn :: Player
    }


newGame :: Game
newGame = Game emptyBoard (const' 9) (const' 0) White

data Move = Move
    { moveFrom :: Maybe Coords
    , moveTo :: Coords
    }
    deriving (Eq,Ord,Show)

applyMove :: Move -> Game -> (Game,Maybe[Coords])
applyMove Move{..} = do
    Game{..} <- id
    let unplayed' = case moveFrom of
            Just _ -> unplayed
            Nothing -> modPlayer playersTurn (+ (-1)) unplayed
    let board0 = case moveFrom of
            Nothing -> board
            Just c -> setBoard c Nothing board
        board' = setBoard moveTo (Just playersTurn) board0
    let game' = Game
            { unplayed = unplayed'
            , board = board'
            , playersTurn = opponent playersTurn
            , ..
            }
    return (game',
        if isMill board' moveTo
        then Just (capturable game')
        else Nothing
        )

capturable :: Game -> [Coords]
capturable Game{..} = do
    let cs = playerFields board playersTurn
        notinmill = List.filter (not . isMill board) cs
    case notinmill of
        [] -> cs
        _ -> notinmill



capture :: Coords -> Game -> Game
capture c = do
    Game{..} <- id
    let Just p = lookupBoard board c
    return Game { board = setBoard c Nothing board
                , captured = modPlayer p (+1) captured
                , ..
                }

neighboursOf :: Coords -> [Coords]
neighboursOf (fromEnum->coords) = fmap toEnum $
    let (s,cp) = coords `divMod` 8
     in [s*8+((cp+1)`mod`8),s*8+((cp+7)`mod`8)]<>
        [ss*8+cp | cp .&. 1 == 1, ss<-if s==1 then [0,2] else [1]]


allowedMoves :: Game -> [Move]
allowedMoves Game{..} = do
    guard $ captured `apply` White < 7
    guard $ captured `apply` Black < 7
    moveFrom <- if unplayed `apply` playersTurn > 0
                then [Nothing]
                else [Just c|c<-playerFields board playersTurn]
    let last3 = captured `apply` playersTurn == 6
    moveTo <- case moveFrom of
                Nothing -> emptyFields board
                Just _ | last3 -> emptyFields board
                Just from -> [n|n<-neighboursOf from,Nothing==lookupBoard board n]
    return $ Move{..}
    

instance Show Game where
    show Game{..} = let op = opponent playersTurn
                        uo = apply unplayed op
                        co = apply captured op
                        up = apply unplayed playersTurn
                        cp = apply captured playersTurn
                        wo = 23-uo-cp
                        wp = 23-up-co
                        uo' = replicate uo (show op)
                        co' = replicate co (show op)
                        up' = replicate up (show playersTurn)
                        cp' = replicate cp (show playersTurn)
                        wo' = mconcat $ ["  "] <> uo' <> replicate wo " " <> cp'
                        wp' = mconcat $ ["  "] <> co' <> replicate wp " " <> up'
                     in wo'<>"\n"<>show board <> wp' <> "\n"

instance Show Player where
    show Black = "○" -- ⚫
    show White = "●" -- ⚪

instance Show Board where
-- ╔═════╦═════╗
-- ║ ╔═══╬═══╗ ║
-- ║ ║ ╔═╩═╗ ║ ║
-- ╠═╬═╣   ╠═╬═╣
-- ║ ║ ╚═╦═╝ ║ ║
-- ║ ╚═══╬═══╝ ║
-- ╚═════╩═════╝
-- ●○
    show board = Vector.toList (plainboard Vector.// mods)
        where
        mods :: [(Int,Char)]
        mods = do
            c <- [minBound..maxBound]
            Just p <- [lookupBoard board c]
            let k = pos c
            [(k,List.head (show p))]<>
                [(k-1,'╸')|plainboard Vector.! (k-1) == '╾']<>
                [(k+1,'╺')|plainboard Vector.! (k+1) == '╼']


        pos :: Coords -> Int
        pos (fromEnum->c) = y*28+x+1
            where
                stage = (3-(c `shiftR` 3))
                x0 = [-1,0,1,1,1,0,-1,-1]!!(c.&.7)
                y0 = [-1,-1,-1,0,1,1,1,0]!!(c.&.7)
                x = (3+stage*x0)*4
                y = (3+stage*y0)*2

        plainboard :: Vector Char
        plainboard = Vector.fromList $ unlines
            [ " ┌╼━━━━━━━━━╾┬╼━━━━━━━━━╾┐ "
            , " ┃           ┃           ┃ "
            , " ┃   ┌╼━━━━━╾┼╼━━━━━╾┐   ┃ "
            , " ┃   ┃       ┃       ┃   ┃ "
            , " ┃   ┃   ┌╼━╾┴╼━╾┐   ┃   ┃ "
            , " ┃   ┃   ┃       ┃   ┃   ┃ "
            , " ├╼━╾┼╼━╾┤       ├╼━╾┼╼━╾┤ "
            , " ┃   ┃   ┃       ┃   ┃   ┃ "
            , " ┃   ┃   └╼━╾┬╼━╾┘   ┃   ┃ "
            , " ┃   ┃       ┃       ┃   ┃ "
            , " ┃   └╼━━━━━╾┼╼━━━━━╾┘   ┃ "
            , " ┃           ┃           ┃ "
            , " └╼━━━━━━━━━╾┴╼━━━━━━━━━╾┘ "
            ]

-- ┌╼━━━━━━━━━╾┬╼━━━━━━━━━╾┐
-- ┃           ┃           ┃
-- ┃   ┌╼━━━━━╾┼╼━━━━━╾┐   ┃
-- ┃   ┃       ┃       ┃   ┃
-- ┃   ┃   ┌╼━╾┴╼━╾┐   ┃   ┃
-- ┃   ┃   ┃       ┃   ┃   ┃
-- ├╼━╾┼╼━╾┤       ├╼━╾┼╼━╾┤
-- ┃   ┃   ┃       ┃   ┃   ┃
-- ┃   ┃   └╼━╾┬╼━╾┘   ┃   ┃
-- ┃   ┃       ┃       ┃   ┃
-- ┃   └╼━━━━━╾┼╼━━━━━╾┘   ┃
-- ┃           ┃           ┃
-- └╼━━━━━━━━━╾┴╼━━━━━━━━━╾┘

-- ━┃┏┓┗┛┣┫┳┻╋ ['╸','╺']
-- └┘├┤┬┴┼┌┐│─
-- ╾┤╼

-- ═║╔╗╚╝╠╣╦╩╬

-- ─━│┃┄┅┆┇┈┉┊┋┌┍┎┏┐┑┒┓└┕┖┗┘┙┚┛├┝┞┟┠┡┢┣┤┥┦┧┨┩┪┫┬┭┮┯┰┱┲┳┴┵┶┷┸┹┺┻
-- ┼┽┾┿╀╁╂╃╄╅╆╇╈╉╊╋╌╍╎╏═║╒╓╔╕╖╗╘╙╚╛╜╝╞╟╠╡╢╣╤╥╦╧╨╩╪╫╬╭╮╯╰╱╲╳╴╵╶╷╸╹╺╻╼╽╾╿
-- ▀▁▂▃▄▅▆▇█▉▊▋▌▍▎▏▐░▒▓▔▕▖▗▘▙▚▛▜▝▞▟■□▢▣▤▥▦▧▨▩
-- ▪▫▬▭▮▯▰▱▲△▴▵▶▷▸▹►▻▼▽▾▿◀◁◂◃◄◅◆◇◈

main :: IO ()
main = rep newGame


ai_weight :: Game -> Rational
ai_weight game =
    let a = List.length $ allowedMoves game
        b = List.length $ allowedMoves game{playersTurn=opponent(playersTurn game)}
        ca = captured game `apply`(playersTurn game)
        cb = captured game `apply`(opponent (playersTurn game))
     in fromIntegral (a+5*cb)/fromIntegral (a+b+5*(ca+cb)) - (1%2)

ai_best_step :: Int -> Game -> (Rational,Game)
ai_best_step 0 game = (ai_weight game,game)
ai_best_step n game =
    let gs :: [(Rational,Game)]
        gs = do
            o <- ai_steps game
            let (!r,_) = ai_best_step (n-1) o
            return (-r,o)
     in if List.null gs then ai_best_step 0 game else
            List.maximumBy (compare `on` fst) gs

ai_run :: Game -> Game
ai_run game = snd $ ai_best_step 6 game


removeDuplicates :: [Game] -> [Game]
removeDuplicates gs = IntMap.elems . IntMap.fromList $ do
    g <- gs
    let sym = boardSymmetry $ board g
    pure (IntSet.findMin sym,g)

ai_steps :: Game -> [Game]
ai_steps game = removeDuplicates do
    m <- allowedMoves game :: [Move]
    let (game',mcap) = applyMove m game :: (Game,Maybe[Coords])
    case mcap of
        Nothing -> [game']
        Just cap -> [capture c game'|c<-cap]


rep_ai game = rep $ ai_run game

rep :: Game -> IO ()
rep game = do
    print game    
    let ms = allowedMoves game :: [Move]
    if List.null ms then putStrLn "Game finished." else do
    if playersTurn game == Black
        then rep_ai game
        else rep_human game

rep_human :: Game -> IO ()
rep_human game = do
    let ms = allowedMoves game :: [Move]
    ln<-getLine
    let m = case readsPrec 0 ln of
                [(t::Coords,"")] -> Right $ Move Nothing t
                [(f::Coords,'-':(readsPrec 0->[(t::Coords,"")]))] -> Right $ Move (Just f) t
                _ -> Left "wrong input"
    let m' = case m of
                Right m | m `List.elem` ms -> Right m
                Right _ -> Left "move not allowed"
                _ -> m
    case m' of
        Left x -> do
            putStrLn x
            rep game
        Right m -> do
            print m
            let (game',mcap) = applyMove m game :: (Game,Maybe[Coords])
            print mcap
            case mcap of
                Nothing -> rep game'
                Just cap -> repcap cap game'
                
repcap :: [Coords] -> Game -> IO ()
repcap caps game = do
    putStrLn "Which piece to capture?"
    ln<-getLine
    let m = case readsPrec 0 ln of
                [(c::Coords,"")] -> Right $ c
                _ -> Left "wrong input"
    let m' = case m of
                Right m | m `List.elem` caps -> Right m
                Right _ -> Left "uncapturable"
                _ -> m
    case m' of
        Left x -> do
            putStrLn x
            repcap caps game
        Right m -> do
            rep $ capture m game   











