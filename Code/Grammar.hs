module Code.Grammar where

data GrammarTerminals = Semicolon | Derives | AlsoDerives | Epsilon | Symbol String deriving (Show, Eq, Ord)

data GrammarNonTerminals = Grammar | ProductionList | ProductionList' | ProductionSet | ProductionSet' | Rhs | SymbolList deriving (Show, Eq, Ord)
