module Code.Lexer where


import           Code.Grammar
import           Control.Monad
import           Data.Char     (isAlphaNum)
import           Data.Foldable (traverse_)
import           System.IO


lexOne :: String -> GrammarTerminals
lexOne ";"       = Semicolon
lexOne ":"       = Derives
lexOne "|"       = AlsoDerives
lexOne "epsilon" = Epsilon
lexOne "Epsilon" = Epsilon
lexOne "EPSILON" = Epsilon
lexOne sym =
  if all isAlphaNum sym then Symbol sym else error $ "Cannot lex: " ++ sym

-- parse :: [Either Terminals String] -> []

lexer :: String -> [GrammarTerminals]
lexer = map lexOne . words

process :: String -> IO ()
process = traverse_ print . lexer

-- parser :: [GrammarTerminals] -> GrammarAST
-- parser = undefined
--   where
--     start = parseHelper GGrammar

-- parseHelper :: GrammarNonTerminals -> [GrammarTerminals] -> IO ()
-- parseHelper nT
