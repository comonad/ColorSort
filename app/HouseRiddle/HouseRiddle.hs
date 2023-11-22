{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PackageImports #-}


-- | https://www.popularmechanics.com/science/math/a24620/riddle-of-the-week-10-einsteins-riddle/

module Main where


import Data.Map.Strict as Map
import Data.Set as Set
import Data.List as List
import Data.Function
import Data.Foldable

import "transformers" Control.Monad.Trans.State.Lazy as State

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

newtype RelationshipDB = RelationshipDB_ (Map Token (Map Token RelationshipType))
    deriving Eq

emptyDB :: RelationshipDB
emptyDB = RelationshipDB_ $ Map.fromList [ (token,Map.singleton token IsSame) | token<-[minBound..maxBound]]


queryDB :: (Token,(),Token) -> RelationshipDB -> Maybe RelationshipType
queryDB (a,(),b) (RelationshipDB_ m) = Map.lookup b (m Map.! a)

setValueDB :: Relationship -> RelationshipDB -> RelationshipDB
setValueDB (a,r,b) db = setVal a r b $ setVal b r a $ db
    where
        setVal a r b (RelationshipDB_ m) = RelationshipDB_ $ Map.adjust (Map.insert b r) a $ m

insertRelationship :: Relationship -> RelationshipDB -> Maybe RelationshipDB
insertRelationship (a,r,b) db = do
    let oldR = queryDB (a,(),b) db :: Maybe RelationshipType
    case (oldR) of
        (Just o) | r==o -> Just db -- do nothing
        (Just _) -> Nothing -- won't do
        (Nothing) -> Just $ setValueDB (a,r,b) db


fixDB :: RelationshipDB -> Maybe RelationshipDB
fixDB db = do
    db' <- fixDB_ db
    if db'==db then return db else fixDB db'

fixDB_ :: RelationshipDB -> Maybe RelationshipDB
fixDB_ db = State.execStateT (traverse_ prog $ allThatAreSame db) db
    where
        prog :: Set Token -> StateT RelationshipDB Maybe ()
        prog (Set.toAscList->sames) = do
            let rs = [(a,IsSame,b) | (a:bs)<-List.tails sames, b<-bs]
            State.modifyM $ \db->foldlM (flip insertRelationship) db rs

            db<-State.get
            let notsame = Set.toAscList $ Set.unions[getAllThatAreNotSame s db | s<-sames] :: [Token]
            let ns = [(a,IsNotSame,b) | a<-sames, b<-notsame]
            State.modifyM $ \db-> foldlM (flip insertRelationship) db ns


getAllThatAreNotSame :: Token -> RelationshipDB -> Set Token
getAllThatAreNotSame a (RelationshipDB_ m) =  Set.fromAscList [ b | (b,IsNotSame) <- Map.toAscList (m Map.! a) ]

allThatAreSame :: RelationshipDB -> [Set Token] -- no singleton sets
allThatAreSame (RelationshipDB_ m) = State.evalState prog (Map.empty,Map.empty)
    where
        lookupNext :: Token -> State (Map Token Token,Map Token Token) (Maybe Token)
        lookupNext a = State.gets $ \(prev,next)->Map.lookup a next
        lookupPrev :: Token -> State (Map Token Token,Map Token Token) (Maybe Token)
        lookupPrev b = State.gets $ \(prev,next)->Map.lookup b prev

        chain :: Token -> Token -> State (Map Token Token,Map Token Token) ()
        chain a b = State.modify $ \(prev,next)->(Map.insert b a prev,Map.insert a b next)
        unchain :: Token -> Token -> State (Map Token Token,Map Token Token) ()
        unchain a b = State.modify $ \(prev,next)->(Map.delete b prev,Map.delete a next)

        registerAsSame :: Token -> Token -> State (Map Token Token,Map Token Token) ()
        registerAsSame a b | a<b = do
            maybe_a_next <- lookupNext a
            maybe_b_prev <- lookupPrev b
            case (maybe_a_next,maybe_b_prev) of
                (Just a_next,_          ) |  a_next == b       -> return ()
                (Just a_next,Just b_prev) |  a_next == b_prev  -> return ()
                (_          ,Just b_prev) |  a      == b_prev  -> return ()
                (Just a_next,Just b_prev) |  a_next <  b_prev  -> registerAsSame a_next b_prev
                (Just a_next,_          ) |  a_next <  b       -> registerAsSame a_next b
                (_          ,Just b_prev) |  a      <  b_prev  -> registerAsSame a      b_prev
                (Just a_next,Just b_prev) -> do
                    unchain a a_next
                    unchain b_prev b
                    registerAsSame b_prev a
                    registerAsSame a b
                    registerAsSame b a_next
                (Just a_next,Nothing) -> do
                    unchain a a_next
                    registerAsSame a b
                    registerAsSame b a_next
                (Nothing,Just b_prev) -> do
                    unchain b_prev b
                    registerAsSame b_prev a
                    registerAsSame a b
                (Nothing,Nothing) -> do
                    chain a b

        prog :: State (Map Token Token,Map Token Token) [Set Token]
        prog = do
            let sames = Set.toList . Set.fromList $ [ {- a is in bs contained -} [ b | (b,IsSame) <- Map.toAscList ma ] | (a,ma)<-Map.toAscList m] :: [[Token]]
            let f (a:x@(b:_)) = registerAsSame a b >> f x
                f _ = return ()
            traverse_ f sames
            (prev,next) <- State.get
            let starts = Map.keys $ next Map.\\ prev :: [Token]
                double a = (a,a)
                f a = double <$> Map.lookup a next
            return [Set.fromAscList $ s:List.unfoldr f s | s<-starts]








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
    ]


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
    return ()





