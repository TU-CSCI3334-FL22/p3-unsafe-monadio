module Reader where

type Terminal = String
type NonTerminal = String
type SymbolTable = [(String, Int)] --Map of strings to ints 
data Token = Undefined
data IR = Undefined2

grammarScan :: String -> ([Token], SymbolTable)
grammarScan = undefined

--IR, symbol table, list of non-terminals You can move the generation of the symbol table to here if you want.
grammarParse :: ([Token], SymbolTable) -> (IR, SymbolTable, [NonTerminal]) 
grammarParse = undefined
