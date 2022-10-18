module Code.Grammar where

data GrammarTerminals = Semicolon | Derives | AlsoDerives | Epsilon | Symbol String deriving (Show, Eq, Ord)

data GrammarNonTerminals = GGrammar | GProductionList | GProductionList' | GProductionSet | GProductionSet' | GRhs | GSymbolList deriving (Show, Eq, Ord)

-- A -> Babc | C
data ProductSet = GrammarProductionSet Lhs [Rhs] deriving (Show, Eq, Ord)

-- Babc
newtype Rhs = Rhs [String] deriving (Show, Eq, Ord)
-- A
newtype Lhs = Lhs String deriving (Show, Eq, Ord)
newtype GrammarAST =
  AST [ProductSet] deriving (Show, Eq, Ord)
