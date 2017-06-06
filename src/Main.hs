import Language.B.Parser
import Language.B.CodeGen

import System.Environment

main :: IO ()
main = do
    (fileName:_) <- getArgs
    Just prgm <- parseProgram fileName
    genProgram prgm