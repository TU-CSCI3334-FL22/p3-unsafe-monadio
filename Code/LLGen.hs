module Code.LLGen where
import           Code.Grammar    (GrammarAST, NonTerminal)
import           Code.Reader
import           Data.Map.Strict (Map)



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
makeNextTable = undefined

showTables ::  (FirstTable, FollowTable, NextTable) -> String
showTables = undefined

toYaml ::  (FirstTable, FollowTable, NextTable) -> Maybe String
toYaml = undefined

fixLL :: (GrammarAST, [NonTerminal])  -> (GrammarAST, [NonTerminal])
fixLL = undefined
