module Code.LLGen where
import           Code.Grammar (GrammarAST)
import           Code.Reader

type FirstTable = [(String, [Int])]
type FollowTable = [(NonTerminal, [Int])]
type NextTable = [(Int, [Int])]

makeTables :: (GrammarAST, [NonTerminal]) -> (FirstTable, FollowTable, NextTable)
makeTables = undefined

showTables ::  (FirstTable, FollowTable, NextTable) -> String
showTables = undefined

toYaml ::  (FirstTable, FollowTable, NextTable) -> Maybe String
toYaml = undefined

fixLL :: (GrammarAST, [NonTerminal])  -> (GrammarAST, [NonTerminal])
fixLL = undefined
