module LLGen where
import Reader

type FirstTable = [(String, [Int])]
type FollowTable = [(NonTerminal, [Int])]
type NextTable = [(Int, [Int])]

makeTables :: (IR, SymbolTable, [NonTerminal]) -> (FirstTable, FollowTable, NextTable)
makeTables = undefined

showTables ::  (FirstTable, FollowTable, NextTable) -> String
showTables = undefined

toYaml ::  (FirstTable, FollowTable, NextTable) -> Maybe String
toYaml = undefined

fixLL :: (IR, SymbolTable, [NonTerminal])  -> (IR, SymbolTable, [NonTerminal]) 
fixLL = undefined
