import           Code.Lexer         (process)
import qualified Code.Main
import           Code.Stuff         (test)
import           System.Environment


main :: IO ()
-- main = Code.Main.main
main = do
  args <- getArgs
  s <- readFile $ head args

  process s
