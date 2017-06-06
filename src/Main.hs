import Language.B.Parser
import Language.B.CodeGen

import System.Environment

main :: IO ()
main = do
    (fileName:_) <- getArgs
    prgm' <- parseProgram fileName
    case prgm' of
        Just prgm -> genProgram prgm
        _ -> return ()