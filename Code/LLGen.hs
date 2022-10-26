module Code.LLGen where
import           Code.Reader
import           Data.Map.Strict (Map,(!), fromList)
import qualified Data.Map.Strict as Map
import           Code.Grammar (GrammarAST (..), Rhs (..), Lhs (..), ProductSet (..), NonTerminal)
import Data.List



type FirstTable = Map String [String]
type FollowTable = Map NonTerminal [String]
type NextTable = [(String, [String])]

makeTables :: (GrammarAST, [NonTerminal]) -> (FirstTable, FollowTable, NextTable)
makeTables ir = (first_table, follow_table, next_table)
  where
    first_table = makeFirstTable ir
    follow_table = makeFollowTable ir first_table
    next_table = makeNextTable ir first_table follow_table

makeFirstTable :: (GrammarAST, [NonTerminal]) -> FirstTable
makeFirstTable = undefined


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
              Nothing -> error "aaaaaaaaaaa"
              Just lst -> lst
    newFollowTable = fst $ foldr (\x b -> followAux x (snd b) first_table (fst b) nt) (follow_table, trailer) rhs

followAux :: String -> [String] -> FirstTable -> FollowTable -> [NonTerminal] -> (FollowTable, [String])
followAux rhs trailer first_table follow_table nt = if rhs `elem` nt then let newTable = Map.insertWith (++) rhs trailer follow_table
                                                                              newTrailer = if "" `elem` firstB then trailer ++ filter (/="") firstB else firstB 
                                                                              in (newTable, newTrailer) 
                                                    else  (follow_table, firstB)
  where
    firstB = case Map.lookup rhs first_table of 
                  Nothing -> error "???"
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
  let firstBs = nub $ concat $ map (\x -> lookUpVal x fstTbl) lst
  in if "" `elem` firstBs then 
       (a, firstBs ++ (lookUpVal a followTbl))
     else (a,firstBs)

lookUpVal :: (Eq a, Ord a) => a -> Map a v ->  v
lookUpVal = flip (!)
-- lookUpVal k (x:xs) = 
--   if fst x == k then snd x -- Just (snd x)
--   else lookUpVal k xs





showTables ::  (FirstTable, FollowTable, NextTable) -> String
showTables = undefined

toYaml ::  (FirstTable, FollowTable, NextTable) -> Maybe String
toYaml = undefined

fixLL :: (GrammarAST, [NonTerminal])  -> (GrammarAST, [NonTerminal])
fixLL = undefined
