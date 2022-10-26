{-# LANGUAGE TupleSections #-}
module Code.Grammar where
import           Data.Foldable  (traverse_)
import           Data.Semigroup ((<>))
import           Data.Set       (Set, fromList)

type Terminal = String
type NonTerminal = String

data GrammarTerminals = Semicolon | Derives | AlsoDerives | Epsilon | Symbol String deriving (Show, Eq, Ord)

-- data GrammarNonTerminals = GGrammar | GProductionList | GProductionList' | GProductionSet | GProductionSet' | GRhs | GSymbolList deriving (Show, Eq, Ord)


-- A -> Babc | C
data ProductSet = GrammarProductionSet Lhs [Rhs] deriving (Show, Eq, Ord)

-- Babc
newtype Rhs = Rhs [String] deriving (Show, Eq, Ord)
-- A
newtype Lhs = Lhs String deriving (Show, Eq, Ord)
newtype GrammarAST =
  AST [ProductSet] deriving (Show, Eq, Ord)

allSymbols :: GrammarAST -> Set String
allSymbols (AST sets) = foldMap (\(GrammarProductionSet (Lhs lhs) rhs_ls) -> fromList $ lhs:foldMap (\(Rhs ls) -> ls) rhs_ls) sets

allProductions :: GrammarAST -> [(Lhs, Rhs)]
allProductions (AST sets) = do
  (GrammarProductionSet lhs rhs_ls) <- sets
  map (lhs,) rhs_ls


padder :: Int -> String -> String
padder n s = s ++ replicate padding ' '
  where
    padding = max 0 (n - length s)

nicePrintProductionSet :: ProductSet -> IO ()
nicePrintProductionSet (GrammarProductionSet (Lhs lhs) (rhs:rhsRest)) = do
  let
    helper (Rhs [])      = "epsilon"
    helper (Rhs symbols) = unwords symbols

  putStrLn $ padder 10 lhs <> " : " <> helper rhs
  traverse_ (\r -> putStrLn (padder 10 "" <> " | " <> helper r)) rhsRest
  putStrLn $ padder 10 "" <> " ;"


nicePrint :: GrammarAST -> IO ()
nicePrint (AST sets) = traverse_ nicePrintProductionSet sets

collectNonTerminals :: GrammarAST -> [String]
collectNonTerminals (AST sets) = map (\(GrammarProductionSet (Lhs lhs) _) -> lhs) sets

