import           Code.Lexer         (process)
import           Code.Stuff         (test)
import           System.Environment


main :: IO ()
main = do
  args <- getArgs
  s <- readFile $ head args

  process s
