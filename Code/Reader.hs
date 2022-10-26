module Code.Reader where

import           Code.Grammar (GrammarAST, NonTerminal, collectNonTerminals)
import           Code.Lexer

-- type SymbolTable = [(String, Int)] --Map of strings to ints
data Token = Undefined

-- grammarScan :: String -> ([Token], SymbolTable)
-- grammarScan = undefined

--IR, symbol table, list of non-terminals You can move the generation of the symbol table to here if you want.
grammarLexAndParse :: String -> (GrammarAST, [NonTerminal])
grammarLexAndParse content = (ast, nonTerminals)
  where
    ast = parser content
    nonTerminals = collectNonTerminals ast


