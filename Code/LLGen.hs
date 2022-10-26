module Code.LLGen where

import           Code.Grammar    (GrammarAST (AST), Lhs (Lhs), NonTerminal,
                                  ProductSet (..), Rhs (Rhs), allProductions,
                                  allSymbols, padder)
import           Code.Reader
import           Data.List
import           Data.List       (intersperse, (\\))
import           Data.Map.Strict (Map, fromList, (!))
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromMaybe)
import           Data.Semigroup  ((<>))
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Debug.Trace     (trace, traceShow, traceShowId)






type FirstTable = Map String (Set String)
type FollowTable = Map NonTerminal [String]
type NextTable = [(String, [String])]

eof :: String
eof = "eof"

fixedPoint :: Eq a => a -> (a -> a) -> a
fixedPoint s f
  | s == s'   = s'
  | otherwise = fixedPoint s' f
  where
    s' = f s

makeTables :: (GrammarAST, [NonTerminal]) -> (FirstTable, FollowTable, NextTable)
makeTables ir = (first_table, follow_table, next_table)
  where
    first_table = makeFirstTable ir
    follow_table = makeFollowTable ir first_table
    next_table = makeNextTable ir first_table follow_table


makeFirstTable :: (GrammarAST, [NonTerminal]) -> FirstTable
makeFirstTable (AST sets, non_term_ls) = fixedPoint start compute_once
  where
    non_terms = Set.fromList non_term_ls
    all_symbols = allSymbols $ AST sets
    terms = Set.difference all_symbols non_terms

    terms_with_eof_e = Set.insert eof $ Set.insert "" terms -- epsilon is ""

    all_productions = allProductions $ AST sets

    first_terms = fromList $ map (\a -> (a,Set.singleton a)) $ Set.toList terms_with_eof_e

    start = first_terms <> fromList ([(nt, mempty) | nt <- non_term_ls])

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
              leading_b_i_with_epsilon = takeWhile (\bi -> elem "" $ first_table ! bi) bs

              rhs_almost_final = rhs <> foldMap to_set_no_eps leading_b_i_with_epsilon

              did_go_to_last = length bs == length leading_b_i_with_epsilon

              -- if included b_k && eps in First(b_k), add eps to rhs
              rhs_final = (if did_go_to_last && elem "" (first_table ! fromMaybe "" (safeLast bs)) then Set.insert "" else id) rhs_almost_final


            in (a, (first_table ! a) <> rhs_final) -- trace (a ++ " -> " ++ show rhs_final)


safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (a:_) = Just a

safeLast :: [a] -> Maybe a
safeLast []     = Nothing
safeLast [a]    = Just a
safeLast (_:ls) = safeLast ls

makeFollowTable :: (GrammarAST, [NonTerminal]) -> FirstTable -> FollowTable
makeFollowTable (AST ast, nt) first_table = undefined
  where
    init_map = (fromList $ [(nt', []) | nt' <- nt])
    (GrammarProductionSet (Lhs top_level) lst) = head ast
    newLst = Map.insert top_level ["eof"] init_map
    productions = getRules (AST ast)
    newFollowTable = foldl (\t p -> followRule p first_table t nt) newLst productions



followRule :: (Lhs, Rhs) -> FirstTable -> FollowTable -> [NonTerminal] -> FollowTable
followRule (Lhs lhs, Rhs rhs) first_table follow_table nt = newFollowTable
  where
    trailer = case Map.lookup lhs follow_table of
              Nothing  -> error "aaaaaaaaaaa"
              Just lst -> lst
    newFollowTable = fst $ foldr (\x b -> followAux x (snd b) first_table (fst b) nt) (follow_table, trailer) rhs

followAux :: String -> [String] -> FirstTable -> FollowTable -> [NonTerminal] -> (FollowTable, [String])
followAux rhs trailer first_table follow_table nt = if rhs `elem` nt then let newTable = Map.insertWith (++) rhs trailer follow_table
                                                                              newTrailer = if "" `elem` firstB then trailer ++ filter (/="") firstB else firstB
                                                                              in (newTable, newTrailer)
                                                    else  (follow_table, firstB)
  where
    firstB = Set.toList $ case Map.lookup rhs first_table of
                  Nothing  -> error "???"
                  Just lst -> lst


makeNextTable :: (GrammarAST, [NonTerminal]) -> FirstTable -> FollowTable -> NextTable
makeNextTable (ast,lst) fstTbl followTbl =
  let ruleSets = getRules ast
  in map (\(l,r) -> nextOfRule l r fstTbl followTbl ) ruleSets

getRules :: GrammarAST -> [(Lhs, Rhs)]
getRules (AST ps) = concat $ map (aux) ps
  where aux :: ProductSet -> [(Lhs,Rhs)]
        aux (GrammarProductionSet a b ) = map (\m -> (a, m)) $ b

nextOfRule :: Lhs -> Rhs -> FirstTable -> FollowTable -> (String, [String])
nextOfRule (Lhs a) (Rhs lst) fstTbl followTbl =
  let firstBs = map (\x -> Set.toList $ lookUpVal x fstTbl) lst
  in if all (\x -> "" `elem` x) firstBs then
       (a, nub $ (concat firstBs) ++ (lookUpVal a followTbl))
     else let legitFirstBs = snd $ foldl (\x y -> if fst x then
                                                     ("" `elem` y, snd x ++ y)
                                                  else x) (True,[]) firstBs
      in (a,nub $ filter (/="") legitFirstBs)

lookUpVal :: (Eq a, Ord a) => a -> Map a v ->  v
lookUpVal = flip (!)
-- lookUpVal k (x:xs) =
--   if fst x == k then snd x -- Just (snd x)
--   else lookUpVal k xs





showTables ::  (FirstTable, FollowTable, NextTable) -> String
showTables (fstTable, followTable, _) = "FirstTable:\n" <> showFirstTable fstTable <> "\n\nFollow Table:\n" <> showFollowTable followTable

showFollowTable :: FollowTable -> String
showFollowTable table = unlines rows
  where
    adjust "" = "ε"
    adjust k  = k

    rows = map (\(k,v) -> "\t" <> padder 20 (adjust k) <> " : " <> mconcat (intersperse ", " (map adjust v)) <> "|") $ Map.toList table


showFirstTable :: FirstTable -> String
showFirstTable table = unlines rows
  where
    adjust "" = "ε"
    adjust k  = k

    rows = map (\(k,v) -> "\t" <> padder 20 (adjust k) <> " : " <> mconcat (intersperse ", " (map adjust $ Set.toList v)) <> "|") $ Map.toList table

toYaml ::  (FirstTable, FollowTable, NextTable) -> Maybe String
toYaml = undefined

fixLL :: (GrammarAST, [NonTerminal])  -> (GrammarAST, [NonTerminal])
fixLL = undefined
