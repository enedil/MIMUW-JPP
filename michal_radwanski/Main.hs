import System.Exit (die)
import Control.Monad.Except
import Control.Monad.Identity
import qualified System.Environment (getArgs)
import ParCerber (myLexer, pProgram)
import qualified Interpreter
import qualified TypeChecker
import Control.Monad.Reader
import System.IO

main :: IO ()
main = do
    args <- System.Environment.getArgs
    case args of
        [path] -> readFile path >>= run
        _ -> die "just put a file name as argument"

run :: String -> IO ()
run text = do
    case pProgram $ myLexer text of
        Left str -> hPutStrLn stderr str
        Right prog -> 
            case runExcept $ TypeChecker.typeCheckProgram prog of
                Left err -> hPutStrLn stderr $ "type error failure: " ++ err
                Right _ -> Interpreter.runProg prog
        
