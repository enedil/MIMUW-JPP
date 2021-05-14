import System.Exit (die)
import qualified System.Environment (getArgs)
import ParCerber (myLexer, pProgram)
--import Interpreter
import qualified TypeChecker

main :: IO ()
main = do
    args <- System.Environment.getArgs
    case args of
        [path] -> readFile path >>= run
        _ -> die $ show args

run :: String -> IO ()
run text = do
    case pProgram $ myLexer text of
        Left str -> putStrLn $ show str
        Right prog -> putStrLn (show prog) >> putStrLn (show (TypeChecker.chck prog))
        
