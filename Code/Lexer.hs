module Code.Lexer where


import           Code.Grammar  (GrammarTerminals (..))
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

process :: String -> IO ()
process s = do
  let split = words s

  traverse_ (print . lexOne) split


