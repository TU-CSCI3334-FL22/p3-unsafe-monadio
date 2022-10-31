{-# LANGUAGE LambdaCase #-}
module Code.Lexer where


import           Code.Grammar
import           Control.Monad
import           Control.Monad.Trans.Except       (Except, throwE)
import           Control.Monad.Trans.State.Strict (State)
import           Data.Bifunctor                   (Bifunctor (second))
import           Data.Char                        (isAlphaNum)
import           Data.Foldable                    (traverse_)
import           Data.List.NonEmpty               (NonEmpty)
import           System.IO

type Error = Except String

lexOne :: String -> Error GrammarTerminals
lexOne ";"       = pure Semicolon
lexOne ":"       = pure Derives
lexOne "|"       = pure AlsoDerives
lexOne "epsilon" = pure Epsilon
lexOne "Epsilon" = pure Epsilon
lexOne "EPSILON" = pure Epsilon
lexOne sym =
  if all isAlphaNum sym then pure $ Symbol sym else throwE $ "Cannot lex: " ++ sym

-- parse :: [Either Terminals String] -> []

addSpacesToSemicolon :: String -> String
addSpacesToSemicolon = (>>= \case ';' -> " ; "; c -> [c])

lexer :: String -> Error [GrammarTerminals]
lexer = traverse lexOne . words . addSpacesToSemicolon

parser :: String -> Error GrammarAST
parser = lexer >=> parseGrammarGrammar

throwGrammarError :: [GrammarTerminals] -> String -> Error a
throwGrammarError ls s = throwE $ "Grammar parse failed. Tried parsing " ++ s ++ ", but got unexpected tokens: " ++ show ls

parseGrammarSymbolList' :: [GrammarTerminals] -> Error ([GrammarTerminals], [String])
parseGrammarSymbolList' ls@(Symbol _:_)    = parseGrammarSymbolList ls
parseGrammarSymbolList' ls@(Semicolon:_)   = pure (ls, [])
parseGrammarSymbolList' ls@(AlsoDerives:_) = pure (ls, [])
parseGrammarSymbolList' ls                 = throwGrammarError ls "SymbolList'"

parseGrammarSymbolList :: [GrammarTerminals] -> Error ([GrammarTerminals], [String]) --State [GrammarTerminals] [String]
parseGrammarSymbolList (Symbol s:ls) = do
  (tokFinal, symbols) <- parseGrammarSymbolList' ls
  pure (tokFinal, s:symbols)
parseGrammarSymbolList ls = throwGrammarError ls "SymbolList"


parseGrammarRhs :: [GrammarTerminals] -> Error ([GrammarTerminals], Rhs)
parseGrammarRhs ls@(Symbol _:_) = second Rhs <$> parseGrammarSymbolList ls
parseGrammarRhs (Epsilon:ls)    = pure (ls, Rhs [])
parseGrammarRhs ls              = throwGrammarError ls "Rhs"

parseGrammarProductionSet' :: [GrammarTerminals] -> Error ([GrammarTerminals], [Rhs])
parseGrammarProductionSet' ls@(Semicolon:_) = pure (ls, [])
parseGrammarProductionSet' (AlsoDerives:ls) = do
  (tok2, rhs) <- parseGrammarRhs ls
  (tok3, rhs_rest) <- parseGrammarProductionSet' tok2
  pure (tok3, rhs:rhs_rest)
parseGrammarProductionSet' ls = throwGrammarError ls "ProductionSet'"

parseGrammarProductionSet :: [GrammarTerminals] -> Error ([GrammarTerminals], ProductSet)
parseGrammarProductionSet (Symbol lhs:Derives:ls) = do
  (tok2, rhs) <- parseGrammarRhs ls
  (tok3, rhs_rest) <- parseGrammarProductionSet' tok2
  pure (tok3, GrammarProductionSet (Lhs lhs) (rhs:rhs_rest))
parseGrammarProductionSet ls = throwGrammarError ls "ProductionSet"

parseGrammarProductionList' :: [GrammarTerminals] -> Error ([GrammarTerminals], [ProductSet])
parseGrammarProductionList' [] = pure ([], [])
parseGrammarProductionList' ls@(Symbol _:_) = do
  (tok2, ps) <- parseGrammarProductionSet ls
  case tok2 of
    (Semicolon:tok3) -> do
      (tok4, psList) <- parseGrammarProductionList' tok3
      pure (tok4, ps:psList)
    ls                -> throwGrammarError ls "ProductionList'"
parseGrammarProductionList' ls = throwGrammarError ls "ProductionList'"

parseGrammarProductionList :: [GrammarTerminals] -> Error ([GrammarTerminals], [ProductSet])
parseGrammarProductionList ls@(Symbol _:_) = do
  (tok2, ps) <- parseGrammarProductionSet ls
  case tok2 of
    (Semicolon:tok3) -> do
      (tok4, psList) <- parseGrammarProductionList' tok3
      pure (tok4, ps:psList)
    ls                -> throwGrammarError ls "ProductionList"
parseGrammarProductionList ls = throwGrammarError ls "ProductionList"

parseGrammarGrammar :: [GrammarTerminals] -> Error GrammarAST
parseGrammarGrammar allTokens@(Symbol _:_) = do
  (left_over, psList) <- parseGrammarProductionList allTokens
  case left_over of
    []   -> pure $ AST psList
    toks -> throwE $ "Grammar parse failed. AST has been parse but unexpected tokens are left over: " ++ show toks
parseGrammarGrammar ls = throwGrammarError ls "Grammar"
