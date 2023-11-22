{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE NondecreasingIndentation #-}


-- | https://www.popularmechanics.com/science/math/a24620/riddle-of-the-week-10-einsteins-riddle/

module Main where


import Data.Map.Strict as Map
import Data.Set as Set
import Data.List as List
import Data.Function
import Data.Foldable

import Control.Monad
import "transformers" Control.Monad.Trans.State.Lazy as State
import "transformers" Control.Monad.Trans.Class as Trans
import Prog

{-
There are five houses sitting next to each other on a neighborhood street, as depicted in the picture above.
Each house's owner is of a different nationality.
Each house has different colored walls.
Each house's owner drinks their own specific beverage, smokes their own brand of cigar, and keeps a certain type of pet.
None of the houses share any of these variables—nationality, wall color, beverage, cigar, and pet—they are all unique.

The Englishman lives in the house with red walls.
The Swede keeps dogs.
The Dane drinks tea.
The house with green walls is just to the left of the house with white walls.
The owner of the house with green walls drinks coffee.
The man who smokes Pall Mall keeps birds.
The owner of the house with yellow walls smokes Dunhills.
The man in the center house drinks milk.
The Norwegian lives in the first house.
The Blend smoker has a neighbor who keeps cats.
The man who smokes Blue Masters drinks beer.
The man who keeps horses lives next to the Dunhill smoker.
The German smokes Prince.
The Norwegian lives next to the house with blue walls.
-}


data RelationshipType = IsSame | IsNotSame deriving (Eq,Ord)




type Relationship = (Token,RelationshipType,Token)



data RDB_X a = RDB_X_ { rdb_isSame :: Set a
                      , rdb_isNotSame :: Set a
                      }

newtype RDB a = RDB_ (Map a (Either a (RDB_X a)))

emptyRDB :: RDB a
emptyRDB = RDB_ Map.empty
emptyRDB_X :: a -> RDB_X a
emptyRDB_X a = RDB_X_ (Set.singleton a) Set.empty

fetch_RDB_X :: (Ord a, Monad m) => a -> StateT (RDB a) m (a,RDB_X a)
fetch_RDB_X a = do
    (RDB_ m)<-State.get
    case Map.lookup a m of
        Nothing -> return (a,emptyRDB_X a)
        Just (Right r) -> return (a,r)
        Just (Left a') -> do
            result@(a'',_) <- fetch_RDB_X a'
            when (a'' /= a') $ replace_RDB_X (a,Left a'')
            return result

replace_RDB_X :: (Ord a, Monad m) => (a,Either a (RDB_X a)) -> StateT (RDB a) m ()
replace_RDB_X (a,rdbx) = do
    (RDB_ m)<-State.get
    State.put . RDB_ $ Map.insert a rdbx m

queryRDB :: (Ord a, Monad m) => (a,(),a) -> StateT (RDB a) m (Maybe RelationshipType)
queryRDB (a,(),b) = do
    (_,rdbx_a)<-fetch_RDB_X a
    return $ if Set.member b $ rdb_isNotSame rdbx_a
             then Just IsNotSame
             else if Set.member b $ rdb_isSame rdbx_a
             then Just IsSame
             else Nothing

setValueRDB :: (Ord a) => (a,RelationshipType,a) -> StateT (RDB a) Maybe ()
setValueRDB (a,IsNotSame,b) = do
    (a',rdbx_a)<-fetch_RDB_X a
    (b',rdbx_b)<-fetch_RDB_X b
    Trans.lift $ guard $ a' /= b'
    let rdbx_a' = rdbx_a{rdb_isNotSame=Set.union (rdb_isNotSame rdbx_a) (rdb_isSame rdbx_b)}
    let rdbx_b' = rdbx_b{rdb_isNotSame=Set.union (rdb_isNotSame rdbx_b) (rdb_isSame rdbx_a)}
    replace_RDB_X (a',Right rdbx_a')
    replace_RDB_X (b',Right rdbx_b')
setValueRDB (a,IsSame,b) = do
    (a',rdbx_a)<-fetch_RDB_X a
    (b',rdbx_b)<-fetch_RDB_X b
    if a' == b' then return () else do
    let rdbx_c = RDB_X_ { rdb_isSame = rdb_isSame rdbx_a `Set.union` rdb_isSame rdbx_b
                        , rdb_isNotSame = rdb_isNotSame rdbx_a `Set.union` rdb_isNotSame rdbx_b
                        }
    Trans.lift $ guard $ Set.null $ rdb_isSame rdbx_c `Set.intersection` rdb_isNotSame rdbx_c
    let c' = min a' b'
    let d' = max a' b'
    replace_RDB_X (c', Right rdbx_c)
    replace_RDB_X (d', Left c')



insertRel :: (Ord a) => (a,RelationshipType,a) -> RDB a -> Maybe (RDB a)
insertRel (a,r,b) = State.execStateT $ do
    setValueRDB (a,r,b)

insertRels :: (Ord a) => [(a,RelationshipType,a)] -> (RDB a) -> Maybe (RDB a)
insertRels ns db = foldlM (flip insertRel) db ns

allThatAreSameRDB :: (Ord a) => RDB a -> [Set a]
allThatAreSameRDB (RDB_ m) = [ rdb_isSame rdbx | (Right rdbx)<-Map.elems m ]





------------------------------------------------------------------------------------------------------------------------









pattern IS x <- IS_ (Set.toAscList->x) where
    IS x = IS_ (Set.fromList x)
pattern UNEQ x <- UNEQ_ (Set.toAscList->x) where
    UNEQ x = UNEQ_ (Set.fromList x)


data Relation
    = IS_ (Set Token)
    | UNEQ_ (Set Token)
    | AND [Relation]
    | OR [Relation]
    | TRUE | FALSE
    deriving (Eq,Ord)

instance Show Relation where
    show (IS is) = (\x->"("<>x<>")") $ List.intercalate " == " $ fmap show is
    show (UNEQ is) = (\x->"["<>x<>"]") $ List.intercalate " # " $ fmap show is
    show (AND xs) = (\x->" < "<>x<>" > ") $ List.intercalate " & " $ fmap show xs
    show (OR xs) = (\x->"« "<>x<>" » ") $ List.intercalate "\n |" $ fmap show xs

--rules = [("Englishman","walls red"),("Swede","keeps dogs"),("Dane","drinks tea"),("walls green","left of walls white"),("walls green","drinks coffee"),("smokes Pall Mall","keeps birds"),("walls yellow","smokes Dunhills"),("center house","drinks milk"),("Norwegian","first house"),("smokes Blend","neighbor keeps cats"),("smokes Blue Masters","drinks beer"),("keeps horses","next to smokes Dunhill"),("German","smokes Prince"),("Norwegian","next to walls blue"),("smokes Blend","neighbor drinks water")]

data Token
    = Nationality_Englishman|Nationality_Swede|Nationality_Dane|Nationality_Norwegian|Nationality_German
    | Walls_red|Walls_green|Walls_white|Walls_blue|Walls_yellow
    | Drinks_tea|Drinks_coffee|Drinks_milk|Drinks_beer|Drinks_water
    | Smokes_PallMall|Smokes_Dunhills|Smokes_Blend|Smokes_BlueMasters|Smokes_Prince
    | Keeps_dogs|Keeps_birds|Keeps_cats|Keeps_horses|Keeps_fish
    | House_1|House_2|House_3|House_4|House_5
    deriving (Eq,Ord,Show,Enum,Bounded)

relationGroups :: Relation
relationGroups = AND (UNEQ_ <$> fmap Set.fromList x)
    where
        x :: [[Token]]
        x = List.groupBy ((==)`on`(List.head . show)) [minBound..maxBound]

relationBinds :: Relation
relationBinds = AND [OR[AND (zipWith(\a b->IS[a,b]) p g) | p<-permutations g1] | g<-gr]

    where
        (g1:gr) = [ g | let (AND gs) = relationGroups, (UNEQ g)<-gs ] :: [[Token]]


leftOf :: Token -> Token -> Relation
leftOf l r = OR [ AND[IS[l,h],IS[r,succ h]]  | h<-[House_1 .. House_4]]
neighbourOf :: Token -> Token -> Relation
neighbourOf a b = OR[leftOf a b,leftOf b a]

rules :: Relation
rules = AND $ relationGroups :
    [ IS[Nationality_Englishman,Walls_red] --The Englishman lives in the house with red walls.
    , IS[Nationality_Swede,Keeps_dogs] --The Swede keeps dogs.
    , IS[Nationality_Dane,Drinks_tea] --The Dane drinks tea.
    , Walls_green `leftOf` Walls_white --The house with green walls is just to the left of the house with white walls.
    , IS[Walls_green,Drinks_coffee] --The owner of the house with green walls drinks coffee.
    , IS[Smokes_PallMall,Keeps_birds] --The man who smokes Pall Mall keeps birds.
    , IS[Walls_yellow,Smokes_Dunhills] --The owner of the house with yellow walls smokes Dunhills.
    , IS[House_3,Drinks_milk] --The man in the center house drinks milk.
    , IS[Nationality_Norwegian,House_1] --The Norwegian lives in the first house.
    , Smokes_Blend `neighbourOf` Keeps_cats --The Blend smoker has a neighbor who keeps cats.
    , IS[Smokes_BlueMasters,Drinks_beer] --The man who smokes Blue Masters drinks beer.
    , Keeps_horses `neighbourOf` Smokes_Dunhills --The man who keeps horses lives next to the Dunhill smoker.
    , IS[Nationality_German,Smokes_Prince] --The German smokes Prince.
    , Nationality_Norwegian `neighbourOf` Walls_blue --The Norwegian lives next to the house with blue walls.
    ] ++ [relationBinds]


dedup :: Ord a => [a] -> [a]
dedup = Set.toAscList . Set.fromList

combineAnds :: [Relation] -> Relation
combineAnds [] = TRUE
combineAnds [x] = x
combineAnds xs = if List.elem FALSE ys then FALSE else AND (dedup ys)
    where
        ands (AND as) = as
        ands a = [a]
        ys = mconcat $ ands <$> xs

combineOrs :: [Relation] -> Relation
combineOrs [] = FALSE
combineOrs [x] = x
combineOrs xs = if List.elem TRUE ys then TRUE else OR (dedup ys)
    where
        ors (OR as) = as
        ors a = [a]
        ys = mconcat $ ors <$> xs

reduceAnd :: Relation -> Relation
reduceAnd (AND xs) = combineAnds $ reduceAnd <$> xs
reduceAnd (OR x) = OR $ reduceAnd <$> x
reduceAnd x = x

reduceOr :: Relation -> Relation
reduceOr (OR xs) = combineOrs $ reduceOr <$> xs
reduceOr (AND x) = AND $ reduceOr <$> x
reduceOr x = x


rules' = reduceAnd . reduceOr $ rules


isTokenRel :: Relation -> Bool
isTokenRel (IS_ _) = True
isTokenRel (UNEQ_ _) = True
isTokenRel _ = False

mergeIsUneqOther :: [Set Token] -> [Set Token] -> [Relation] -> Relation
mergeIsUneqOther is isnt other = if contradiction then FALSE else AND (fmap IS_ is' <> fmap UNEQ_ isnt <> other)
    where
        is' = loop_is [] is

        contradiction = List.any contradicts isnt
        contradicts x = List.any (\i->(Set.size $ Set.intersection i x) > 1 ) is'

        ins :: [Set Token] -> Set Token -> [Set Token]
        ins [] i = [i]
        ins (j:js) i | Set.null (Set.intersection i j) = j : ins js i
                     | otherwise = Set.union i j : js
        loop_is :: [Set Token] -> [Set Token] -> [Set Token]
        loop_is result [] = result
        loop_is result (i:is) = loop_is (ins result i) is

mergeIs :: Relation -> Relation
mergeIs (AND (fmap mergeIs->xs)) = mergeIsUneqOther [ is | IS_ is <- xs ] [ is | UNEQ_ is <- xs ] [ is | is@(isTokenRel->False) <- xs ]
mergeIs (OR xs) = OR $ mergeIs <$> xs
mergeIs x = x


unAND (AND x) = x

main = do
    putStrLn ""
    traverse_ print . unAND $ mergeIs rules'
    putStrLn ""
    print mainR
    return ()



mainR = allThatAreSameRDB $ List.head $ Prog.runProg runPR

runPR :: Prog () (RDB Token)
runPR = execStateT (progR $ mergeIs rules') emptyRDB

progR :: Relation -> StateT (RDB Token) (Prog ()) ()
progR (AND rs) = traverse_ progR rs
progR (OR rs) = Trans.lift (foreach rs) >>= progR
progR (TRUE) = return ()
progR (FALSE) = Trans.lift $ mzero
progR (IS ts) = do
    State.modifyM $ Prog.with . insertRels [ (a,IsSame,b) | (a:bs)<-List.tails ts , b<-bs ]
progR (UNEQ ts) = do
    State.modifyM $ Prog.with . insertRels [ (a,IsNotSame,b) | (a:bs)<-List.tails ts , b<-bs ]








