module Code.LLGen where
import           Code.Reader
import           Data.Map.Strict (Map)
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
makeFollowTable = undefined

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

lookUpVal :: Eq a => a -> [(a,v)] ->  v
-- lookUpVal k [] = Nothing
lookUpVal k (x:xs) = 
  if fst x == k then snd x -- Just (snd x)
  else lookUpVal k xs





showTables ::  (FirstTable, FollowTable, NextTable) -> String
showTables = undefined

toYaml ::  (FirstTable, FollowTable, NextTable) -> Maybe String
toYaml = undefined

fixLL :: (GrammarAST, [NonTerminal])  -> (GrammarAST, [NonTerminal])
fixLL = undefined
