{-# LANGUAGE LambdaCase #-}
module Code.Lexer where


import           Code.Grammar
import           Control.Monad
import           Control.Monad.Trans.State.Strict (State)
import           Data.Bifunctor                   (Bifunctor (second))
import           Data.Char                        (isAlphaNum)
import           Data.Foldable                    (traverse_)
import           Data.List.NonEmpty               (NonEmpty)
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

addSpacesToSemicolon :: String -> String
addSpacesToSemicolon = (>>= \case ';' -> " ; "; c -> [c])

lexer :: String -> [GrammarTerminals]
lexer = map lexOne . words . addSpacesToSemicolon

parser :: String -> GrammarAST
parser = parseGrammarGrammar . lexer


parseGrammarSymbolList' :: [GrammarTerminals] -> ([GrammarTerminals], [String])
parseGrammarSymbolList' ls@(Symbol _:_)    = parseGrammarSymbolList ls
parseGrammarSymbolList' ls@(Semicolon:_)   = (ls, [])
parseGrammarSymbolList' ls@(AlsoDerives:_) = (ls, [])

parseGrammarSymbolList :: [GrammarTerminals] -> ([GrammarTerminals], [String]) --State [GrammarTerminals] [String]
parseGrammarSymbolList (Symbol s:ls) = (tokFinal, s:symbols)
  where
    (tokFinal, symbols) = parseGrammarSymbolList' ls

parseGrammarRhs :: [GrammarTerminals] -> ([GrammarTerminals], Rhs)
parseGrammarRhs ls@(Symbol _:_) = second Rhs $ parseGrammarSymbolList ls
parseGrammarRhs (Epsilon:ls)    = (ls, Rhs [])

parseGrammarProductionSet' :: [GrammarTerminals] -> ([GrammarTerminals], [Rhs])
parseGrammarProductionSet' ls@(Semicolon:_) = (ls, [])
parseGrammarProductionSet' (AlsoDerives:ls) = (tok3, rhs:rhs_rest)
  where
    (tok2, rhs) = parseGrammarRhs ls
    (tok3, rhs_rest) = parseGrammarProductionSet' tok2

parseGrammarProductionSet :: [GrammarTerminals] -> ([GrammarTerminals], ProductSet)
parseGrammarProductionSet (Symbol lhs:Derives:ls) = (tok3, GrammarProductionSet (Lhs lhs) (rhs:rhs_rest))
  where
    (tok2, rhs) = parseGrammarRhs ls
    (tok3, rhs_rest) = parseGrammarProductionSet' tok2

parseGrammarProductionList' :: [GrammarTerminals] -> ([GrammarTerminals], [ProductSet])
parseGrammarProductionList' [] = ([], [])
parseGrammarProductionList' ls@(Symbol _:_) = case tok2 of
  (Semicolon:tok3) ->
    let (tok4, psList) = parseGrammarProductionList' tok3
    in (tok4, ps:psList)
  _                -> error "panic!!!!!!!!!!!!!"
  where
    (tok2, ps) = parseGrammarProductionSet ls

parseGrammarProductionList :: [GrammarTerminals] -> ([GrammarTerminals], [ProductSet])
parseGrammarProductionList ls@(Symbol _:_) = case tok2 of
  (Semicolon:tok3) ->
    let (tok4, psList) = parseGrammarProductionList' tok3
    in (tok4, ps:psList)
  _                -> error "panic!!!!!!!!!!!!!"
  where
    (tok2, ps) = parseGrammarProductionSet ls

parseGrammarGrammar :: [GrammarTerminals] -> GrammarAST
parseGrammarGrammar allTokens@(Symbol _:_) =
  case left_over of
    []   -> AST psList
    toks -> error $ "Nooooooooooooo: tokens left over: " ++ show toks
  where
    (left_over, psList) = parseGrammarProductionList allTokens







-- GGrammar | GProductionList | GProductionList' | GProductionSet | GProductionSet' | GRhs | GSymbolList


-- parseHelper :: GrammarNonTerminals -> [GrammarTerminals] -> IO ()
-- parseHelper nT
