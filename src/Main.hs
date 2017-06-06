import Language.B.Parser
import Language.B.CodeGen

import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fileName] -> do
            prgm' <- parseProgram fileName
            case prgm' of
                Just prgm -> genProgram prgm
                _ -> return ()
        _ -> putStrLn "usage: b <filename>"