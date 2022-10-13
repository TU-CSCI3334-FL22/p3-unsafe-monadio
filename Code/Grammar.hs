module Code.Grammar where

data Terminals = Semicolon | Derives | AlsoDerives | Epsilon | Symbol String deriving (Show, Eq, Ord)

data NonTerminals = Grammar | ProductionList | ProductionList' | ProductionSet | ProductionSet' | Rhs | SymbolList deriving (Show, Eq, Ord)
