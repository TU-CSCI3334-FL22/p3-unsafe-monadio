{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Code.LLGen where

import           Code.Grammar                      (GrammarAST (AST), Lhs (Lhs),
                                                    NonTerminal,
                                                    ProductSet (..), Rhs (Rhs),
                                                    allProductions, allSymbols,
                                                    padder)
import           Control.Monad.Trans.Writer.Strict


import           Control.Arrow                     (Arrow (second, (&&&)))
import           Data.Bifunctor                    (Bifunctor (bimap))
import           Data.Foldable                     (sequenceA_, traverse_)
import           Data.List                         (intercalate, intersperse,
                                                    (\\))
import           Data.Map.Strict                   (Map, fromList, (!))
import qualified Data.Map.Strict                   as Map
import           Data.Maybe                        (fromMaybe)
import           Data.Semigroup                    ((<>))
import           Data.Set                          (Set)
import qualified Data.Set                          as Set
import           Data.String                       (IsString (fromString))
import           Data.Tuple                        (swap)
import           Util                              (nubSort)
import          Debug.Trace                        (trace, traceShow, traceShowId)



type FirstTable = Map String (Set String)
type FollowTable = Map NonTerminal (Set String)
type NextTable = Map ProductSet (Set String)
type PredictTable = Map NonTerminal (Set (Int,String))

data WorkListOp = UseWorkList | NoWorkList deriving (Show, Eq, Ord)

eof :: String
eof = "eof"

fixedPoint :: Eq a => a -> (a -> a) -> a
fixedPoint s f
  | s == s'   = s'
  | otherwise = fixedPoint s' f
  where
    s' = f s

makeTables :: WorkListOp -> (GrammarAST, [NonTerminal]) -> (FirstTable, FollowTable, NextTable)
makeTables w ir = (first_table, follow_table, next_table)
  where
    first_table = (case w of
      UseWorkList -> makeFirstTableW
      NoWorkList  -> makeFirstTable) ir
    follow_table = (case w of
      UseWorkList -> makeFollowTableW
      NoWorkList  -> makeFollowTable) ir first_table
    -- follow_table = makeFollowTable ir first_table
    next_table = makeNextTable ir first_table follow_table

-- type NextTable = Map ProductSet (Set Terminal::String)
makePredictTable :: (GrammarAST, [NonTerminal]) -> NextTable -> PredictTable
makePredictTable (ast, nts) nextTable =
  let index2productions = zip [0..] $ map (\(a,b) -> GrammarProductionSet a [b]) $ getRules ast
      -- inde2productions = [(Int, ProductSet)]
      -- emptyTable = map (\nt -> (nt, Set.empty )) nts
      predictTable = foldl (\predT nextRule -> let (GrammarProductionSet (Lhs key) a) = fst nextRule
                                                   index = fst $ head $ filter (\x -> snd x ==  fst nextRule) index2productions
                                                   newElem = let terminals = Set.toList $ snd nextRule
                                                             in Set.fromList $ zip (repeat index ) terminals
                                               in Map.insertWith (<>) key newElem predT ) mempty $ Map.toList nextTable
  in predictTable
  -- where aux :: PredictTable -> a -> b
  --       aux predictTable (ProductSet l)


makeFirstTable :: (GrammarAST, [NonTerminal]) -> FirstTable
makeFirstTable (AST sets, non_term_ls) = fixedPoint start compute_once
  where
    non_terms = Set.fromList non_term_ls
    all_symbols = allSymbols $ AST sets
    terms = Set.difference all_symbols non_terms

    terms_with_eof_e = Set.insert eof $ Set.insert "" terms -- epsilon is ""

    all_productions = allProductions $ AST sets

    first_terms = Map.fromSet Set.singleton terms_with_eof_e

    start = first_terms <> Map.fromSet mempty non_terms

    -- first(e) for e in NT starts at []

    compute_once :: FirstTable -> FirstTable
    compute_once first_table = Map.unionWith (<>) (Map.fromListWith (<>) new) first_table
      where
        -- for each prod
        new = flip map all_productions $
          \(Lhs a, Rhs bs) ->
            let
              -- to_set_no_eps(x) = First(x) // { eps }
              to_set_no_eps = Set.delete "" . (first_table !)

              -- rhs = First(b_1) // { eps }
              -- b_1 is [] if its just epsilon
              rhs = to_set_no_eps $ fromMaybe "" $ safeHead bs

              -- keep leading b_is where epsilon in First(b_i)
              leading_b_i_with_epsilon = takeWhile (elem "" . (first_table !)) bs

              rhs_almost_final = rhs <> foldMap to_set_no_eps leading_b_i_with_epsilon

              did_go_to_last = length bs == length leading_b_i_with_epsilon

              -- if included b_k && eps in First(b_k), add eps to rhs
              rhs_final = (if did_go_to_last && elem "" (first_table ! fromMaybe "" (safeLast bs)) then Set.insert "" else id) rhs_almost_final


            in (a, (first_table ! a) <> rhs_final) -- trace (a ++ " -> " ++ show rhs_final)


(!:?<>@$%) :: (Show k, Ord k) => Map k v -> k -> v
m !:?<>@$% k = case m Map.!? k of
  Nothing -> error $ "key missing: " ++ show k
  Just v  -> v



makeFirstTableW :: (GrammarAST, [NonTerminal]) -> FirstTable
makeFirstTableW (AST sets, non_term_ls) = snd $ compute_once (Map.keysSet all_productions, start)
  where
    non_terms = Set.fromList non_term_ls
    all_symbols = allSymbols $ AST sets
    terms = Set.difference all_symbols non_terms

    terms_with_eof_e = Set.insert eof $ Set.insert "" terms -- epsilon is ""

    all_productions = Map.fromList $ zip ([0..]::[Int]) $ allProductions $ AST sets

    first_terms = Map.fromSet Set.singleton terms_with_eof_e

    start = first_terms <> Map.fromSet mempty non_terms

    nt_to_prod_map :: Map NonTerminal (Set Int)
    nt_to_prod_map = Map.fromListWith (<>) (concatMap (\(k, (Lhs lhs, Rhs rhs)) ->
      let
        referenced_nts = nubSort $ takeWhile (not . (`Set.member` terms)) rhs
      in map (,Set.singleton k) referenced_nts)
      $ Map.toList all_productions) <> Map.fromSet (const mempty) non_terms

    compute_once :: (Set Int, FirstTable) -> (Set Int, FirstTable)
    compute_once (work_list, first_table)
      | Set.null work_list = (mempty, first_table)
      | otherwise = compute_once (new_work_list, updated)
      where
        (prod_idx, popped_work_list) = Set.deleteFindMax work_list

        -- A -> B1 B2 B3 ... Bn
        (Lhs a, Rhs bs) = all_productions !:?<>@$% prod_idx

        -- to_set_no_eps(x) = First(x) // { eps }
        to_set_no_eps = Set.delete "" . (first_table !:?<>@$%)

        -- rhs = First(b_1) // { eps }
        -- b_1 is [] if its just epsilon
        rhs = to_set_no_eps $ fromMaybe "" $ safeHead bs

        -- keep leading b_is where epsilon in First(b_i)
        leading_b_i_with_epsilon = takeWhile (elem "" . (first_table !:?<>@$%)) bs

        rhs_almost_final = rhs <> foldMap to_set_no_eps leading_b_i_with_epsilon

        did_go_to_last = length bs == length leading_b_i_with_epsilon

        -- if included b_k && eps in First(b_k), add eps to rhs
        rhs_final = (if did_go_to_last && elem "" (first_table !:?<>@$% fromMaybe "" (safeLast bs)) then Set.insert "" else id) rhs_almost_final

        updated = Map.insertWith (<>) a ((first_table !:?<>@$% a) <> rhs_final) first_table

        is_the_same = (first_table !:?<>@$% a) == (updated !:?<>@$% a)

        new_work_list = popped_work_list <> if is_the_same then mempty else nt_to_prod_map !:?<>@$% a



safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (a:_) = Just a

safeLast :: [a] -> Maybe a
safeLast []     = Nothing
safeLast [a]    = Just a
safeLast (_:ls) = safeLast ls

makeFollowTableW :: (GrammarAST, [NonTerminal]) -> FirstTable -> FollowTable
makeFollowTableW (AST ast, nt) first_table = snd $ followFixPointW (Map.keysSet all_productions, newLst)
  where
    -- extract values from list of keys
    (!:?<>@$%%) :: (Show k, Ord k, Ord v) => Map k (Set v) -> [k] -> Set v
    m !:?<>@$%% ks = foldl (\s k -> s `Set.union` (m !:?<>@$% k) ) mempty ks  
        
    init_map = fromList $ map (,mempty) nt
    (GrammarProductionSet (Lhs top_level) lst) = head ast
    newLst = Map.insert top_level (Set.singleton "eof") init_map

    non_terms = Set.fromList nt
    all_symbols = allSymbols $ AST ast
    terms = Set.difference all_symbols non_terms
    
    all_productions = Map.fromList $ zip ([0..]::[Int]) $ allProductions $ AST ast

    nt_to_prod_map :: Map NonTerminal (Set Int)
    -- For Follow : Follow(A) `union` U First(B_i) for i = 0 .. n where n is first B that does not contain epsilon
    -- Follow(A) change means that production and any productions start with any nonterminal B_i are back to workList.
    nt_to_prod_map = 
      let followAtoProds = Map.fromListWith (<>) (concatMap (\(k, (Lhs lhs, Rhs rhs)) -> let referenced_nts = [lhs] ++ filter (`elem` nt) rhs in map (,Set.singleton k) referenced_nts) $ Map.toList all_productions) <> Map.fromSet (const mempty) non_terms
      in foldl (\theMap (k, (Lhs lhs, Rhs rhs)) -> 
        let ntRhs = filter (`elem` nt) rhs
            rhsProductions = followAtoProds !:?<>@$%% ntRhs
        in Map.insertWith (<>) lhs  rhsProductions theMap ) followAtoProds $ Map.toList all_productions

    followFixPointW :: (Set Int, FollowTable) -> (Set Int, FollowTable)
    followFixPointW (wkLst,follow_table) 
      | Set.null wkLst = (wkLst,follow_table)
      | otherwise = followFixPointW (new_wkLst, new_follow_table) 
    
      where 
        (prod_idx, popped_work_list) = Set.deleteFindMax wkLst
        (Lhs a, Rhs bs) = all_productions !:?<>@$% prod_idx
        new_follow_table = followRule (Lhs a, Rhs bs) first_table follow_table nt 
        allNT = a : filter (\x -> Set.member x non_terms) bs
        is_the_same = all (==True) $ map (\x -> (follow_table !:?<>@$% x ) == (new_follow_table !:?<>@$% x))  allNT 

        new_wkLst = if is_the_same then  popped_work_list <> mempty 
                    else popped_work_list `Set.union` (nt_to_prod_map !:?<>@$%% allNT)




makeFollowTable :: (GrammarAST, [NonTerminal]) -> FirstTable -> FollowTable
makeFollowTable (AST ast, nt) first_table = fixedPoint newLst followFixPoint
  where
    init_map = fromList $ map (,mempty) nt
    (GrammarProductionSet (Lhs top_level) lst) = head ast
    newLst = Map.insert top_level (Set.singleton "eof") init_map
    productions = getRules (AST ast)

    followFixPoint :: FollowTable -> FollowTable
    followFixPoint newLst = foldl (\t p -> followRule p first_table t nt) newLst productions


    --newFollowTable = foldl (\t p -> followRule p first_table t nt) newLst productions



followRule :: (Lhs, Rhs) -> FirstTable -> FollowTable -> [NonTerminal] -> FollowTable
followRule (Lhs lhs, Rhs rhs) first_table follow_table nt = newFollowTable
  where
    trailer = case Map.lookup lhs follow_table of
              Nothing  -> error "aaaaaaaaaaa"
              Just lst -> lst -- if lhs == "Trailer" || "Trailer" `elem` rhs then traceShow ("for "++ lhs ++ "->" ++ unlines rhs ++ " || follow(A): ", lst) $ lst else lst
    newFollowTable = fst $ foldr (\x b -> followAux x (snd b) first_table (fst b) nt) (follow_table, trailer) rhs

followAux :: String -> Set String -> FirstTable -> FollowTable -> [NonTerminal] -> (FollowTable, Set String)
followAux rhs trailer first_table follow_table nt = if rhs `elem` nt then let newTable = Map.insertWith (<>) rhs trailer follow_table
                                                                              newTrailer = if "" `elem` firstB then trailer <> Set.delete "" (firstB) else firstB
                                                                              in (newTable, newTrailer)
                                                    else  (follow_table, firstB)
  where
    firstB = case Map.lookup rhs first_table of
                  Nothing  -> error "???"
                  Just lst -> lst


makeNextTable :: (GrammarAST, [NonTerminal]) -> FirstTable -> FollowTable -> NextTable
makeNextTable (ast,lst) fstTbl followTbl =
  let ruleSets = getRules ast
  in Map.fromList $ map (\(l,r) -> nextOfRule l r fstTbl followTbl ) ruleSets

getRules :: GrammarAST -> [(Lhs, Rhs)]
getRules (AST ps) = concatMap aux ps
  where aux :: ProductSet -> [(Lhs,Rhs)]
        aux (GrammarProductionSet a b ) = map (a,) b

nextOfRule :: Lhs -> Rhs -> FirstTable -> FollowTable -> (ProductSet, Set String)
nextOfRule (Lhs a) (Rhs lst) fstTbl followTbl =
  let production = GrammarProductionSet (Lhs a) [Rhs lst]
      firstBs = map (`lookUpVal` fstTbl) lst
  in if all (\x -> "" `elem` x) firstBs then
       (production, mconcat firstBs <> lookUpVal a followTbl)
     else let legitFirstBs = snd $ foldl (\x y -> if fst x then
                                                     ("" `elem` y, snd x <> y)
                                                  else x) (True, Set.empty) firstBs
      in (production, Set.delete "" legitFirstBs)

lookUpVal :: (Eq a, Ord a) => a -> Map a v ->  v
lookUpVal = flip (!)
-- lookUpVal k (x:xs) =
--   if fst x == k then snd x -- Just (snd x)
--   else lookUpVal k xs





showTables ::  (FirstTable, FollowTable, NextTable) -> String
showTables (fstTable, followTable, nextTable) = "FirstTable:\n" <> showFirstTable fstTable <> "\n\nFollow Table:\n" <> showFollowTable followTable <> "\n\nNext Table:\n" <> showNextTable nextTable

showNextTable :: NextTable -> String
showNextTable table = unlines rows
  where
    showPS :: ProductSet -> String
    showPS (GrammarProductionSet (Lhs lhs) ((Rhs rhs):_)) = lhs ++ " -> " ++ (case rhs of [] -> "ε"; ls -> unwords ls)

    rows = map ((\(a,b) -> padder 30 a <> " : " <> b) . bimap showPS (unwords . Set.toList)) $ Map.toList table

showFollowTable :: FollowTable -> String
showFollowTable table = unlines rows
  where
    adjust "" = "ε"
    adjust k  = k

    rows = map (\(k,v) -> "\t" <> padder 20 (adjust k) <> " : " <> mconcat (intersperse ", " (map adjust  $ Set.toList v)) <> "|") $ Map.toList table

showFirstTable :: FirstTable -> String
showFirstTable table = unlines rows
  where
    adjust "" = "ε"
    adjust k  = k

    rows = map (\(k,v) -> "\t" <> padder 20 (adjust k) <> " : " <> mconcat (intersperse ", " (map adjust $ Set.toList v)) <> "|") $ Map.toList table


type YamlWriter = Writer [String]
-- data YamlWriter a = Content String a deriving (Show, Eq, Ord)
-- instance Functor YamlWriter where
--   fmap f (Content s a) = Content s $ f a
-- instance Applicative YamlWriter where
--   (Content s f) <*> (Content s' a) = Content (s <> s') (f a)
--   pure a = Content "" a
-- instance Monad YamlWriter where
--   (Content s a) >>= f = Content (s <> s') b
--     where
--       (Content s' b) = f a


keyEntry :: String -> String -> YamlWriter ()
keyEntry key val = tell [key ++ ": " ++ val]

keyEntryMany :: String -> YamlWriter () -> YamlWriter ()
keyEntryMany key values = do
  tell [key ++ ":"]
  tell $ map ("  "<>) $ execWriter values

tellOne = tell . (:[])


-- class ToYaml a where
--   toYaml :: a -> YamlWriter ()

-- instance ToYaml Int where
--   toYaml = tell . (:[]) . show

-- instance {-# OVERLAPS #-} ToYaml String where
--   toYaml = tell . (:[])

-- instance {-# OVERLAPPABLE #-} ToYaml a => ToYaml [a] where
--   toYaml ls = do
--     let
--       inner = execWriter $ traverse_ toYaml ls
--       is_long = any ((>= 2) . length) inner

--     if is_long then
--       undefined
--     else
--       tell ["[" ++ intersperse ", " (map head inner) ++ "]"] -- (sequenceA_ $ intersperse ", " $ )

lsToYaml :: [String] -> String
lsToYaml ls = "[" ++ intercalate ", " ls ++ "]"

singletonMapToYaml :: String -> String -> String
singletonMapToYaml k v = "{" ++ k ++ ": " ++ v ++ "}"

mapToYaml :: Show a => [(String, a)] -> String
mapToYaml ls = "{" ++ intercalate ", " (map (\(a,b) -> a ++ ": " ++ show b) ls) ++ "}"

-- instance ToYaml a => ToYaml (Set a) where
--   toYaml = toYaml . Set.toList

-- instance ToYaml (YamlWriter ()) where
--   toYaml = id


-- toYaml ::  (FirstTable, FollowTable, NextTable) -> Maybe String
-- toYaml = undefined

fixLL :: (GrammarAST, [NonTerminal])  -> (GrammarAST, [NonTerminal])
fixLL = undefined

toYamlAll :: (GrammarAST, [NonTerminal]) -> (FirstTable, FollowTable, NextTable) -> Maybe String
toYamlAll (AST ast, nts) (fstTable, followTable, nextTable) = pure $ unlines $ execWriter $ do
  let
    non_terms = Set.fromList nts
    all_symbols = allSymbols $ AST ast
    terms = Set.difference all_symbols non_terms
    terms_with_eof_e = Set.insert eof $ Set.insert "ε" terms -- epsilon is ""

    (GrammarProductionSet (Lhs top_level) _) = head ast

    all_productions = allProductions $ AST ast

    predict_table = is_LL1 $ makePredictTable (AST ast, nts) nextTable

    is_LL1 :: PredictTable -> Either String PredictTable
    is_LL1 predict_map = traverse check predict_map
      where
        check :: Set (Int, String) -> Either String (Set (Int, String))
        check set = case Map.toList duplicates of
          [] -> Right set
          ls -> Left $ show ls
          where
            x = Map.fromListWith (<>) $ map (second (:[]) . swap) $ Set.toList set
            duplicates = Map.filter ((>1) . length) x

    association_list_to_yaml :: [(Lhs, Rhs)] -> YamlWriter ()
    association_list_to_yaml ls = traverse_ (\(n, (Lhs lhs, Rhs rhs)) -> keyEntry n $ singletonMapToYaml lhs $ lsToYaml rhs) ls_enum
      where
        ls_enum = zip (map show [0..]) ls

    predict_table_to_yaml :: [(NonTerminal, Set (Int, String))] -> YamlWriter ()
    predict_table_to_yaml set = traverse_ (\(nt, set) -> keyEntry nt $ mapToYaml $ map swap $ Set.toList set) set


  tellOne "\n"
  keyEntry "terminals" $ lsToYaml $ Set.toList terms
  keyEntry "non-terminals" $ lsToYaml nts
  keyEntry "eof-marker" eof
  keyEntry "error-marker" "--"
  keyEntry "start-symbol" top_level
  keyEntryMany "productions" $ association_list_to_yaml all_productions

  case predict_table of
    Left err -> tell ["Error: Not LL(1)", err]
    Right predict_table -> keyEntryMany "table" $ predict_table_to_yaml $ Map.toList predict_table


