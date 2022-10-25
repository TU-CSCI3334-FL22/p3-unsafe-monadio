module Code.Main where
import           Code.Grammar          (nicePrint)
import           Code.LLGen
import           Code.Reader
import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Maybe
import           Debug.Trace
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
-- import           System.Random

-- Options record
data Options = Options {
   optHelp     :: Bool
 , optTable    :: Bool
 , optRevise   :: Bool
 , optWorklist :: Bool
 , fname       :: String
 }

defaultOptions :: Options
defaultOptions = Options {
      optHelp = False
    , optTable  = False
    , optRevise = False
    , optWorklist = False
    , fname = ""
 }

options :: [OptDescr (Options -> Options)]
options = [
  Option ['h'] ["help"]   (NoArg  (\opts -> opts { optHelp = True })) "Print a help message and exit.",
  Option ['t'] ["table"]   (NoArg  (\opts -> opts { optTable = True })) "Print YAML tables instead of human-readable output.",
  Option ['r'] ["revise"]   (NoArg  (\opts -> opts { optRevise= True })) "Attempt to revise the grammar if it is not LL(1).",
  Option ['w'] ["worklist"]   (NoArg  (\opts -> opts { optWorklist= True })) "Use the worklist version of First and Follow."
  ]

compilerOpts :: [String] -> Options
compilerOpts argv =
  case getOpt Permute options argv of
     (o,[x],[]) -> foldl (flip id) (defaultOptions {fname = x}) o
     (o,[],[])  -> foldl (flip id) defaultOptions o
     (_,_,[])   -> error (usageInfo header options)
     (_,_,errs) -> error (concat errs ++ usageInfo header options)
  where header = "Usage: ./llgen [OPTION]... [file]"

-- Print help
helpIO :: IO()
helpIO = putStrLn $ usageInfo header options
  where header = "Usage: ./llgen [OPTION]... [file]"

-- Main IO function
main :: IO ()
main = do
  allArgs <- getArgs
  let opts = compilerOpts allArgs
  if optHelp opts || fname opts == "" then helpIO
  else do
    contents <- readFile (fname opts)
    let ir = grammarLexAndParse contents
        improvedIR = if optRevise opts then fixLL ir else ir

    nicePrint (fst ir)
    let tables = makeTables improvedIR --(optWorklist opts)
      in if not $ optTable opts
        then putStrLn $ showTables tables
       else case toYaml tables of
          Nothing  -> error "Not LL(1)"
          Just str -> putStrLn str
