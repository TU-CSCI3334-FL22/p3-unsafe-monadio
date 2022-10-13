module Code.Lexer where


import           Code.Grammar  (NonTerminals, Terminals (..))
import           Control.Monad
import           Data.Char     (isAlphaNum)
import           Data.Foldable (traverse_)
import           System.IO


lexOne :: String -> Either Terminals String
lexOne ";"       = Left Semicolon
lexOne ":"       = Left Derives
lexOne "|"       = Left AlsoDerives
lexOne "epsilon" = Left Epsilon
lexOne "Epsilon" = Left Epsilon
lexOne "EPSILON" = Left Epsilon
lexOne sym =
  if all isAlphaNum sym then Right sym else error $ "Cannot lex: " ++ sym


-- parse :: [Either Terminals String] -> []

process :: String -> IO ()
process s = do
  let split = words s

  traverse_ (print . lexOne) split


