{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.B.CodeGen where

import qualified Language.B.AST as B

import Control.Monad.Except
import LLVM.Context
import LLVM.Module
import LLVM.AST.Global
import qualified LLVM.AST as L

int :: L.Type
int = L.IntegerType 32

genDef :: B.Definition -> L.Definition
genDef (B.Function n ps body) = L.GlobalDefinition L.functionDefaults
  { name = L.Name n
  , parameters = (map genParam ps, False)
  , returnType = int
  , basicBlocks = []
  }

genParam :: String -> L.Parameter
genParam p = L.Parameter int (L.Name p) []

module_ :: String -> L.Module
module_ s = L.defaultModule

toLLVM :: L.Module -> IO ()
toLLVM m = withContext $ \ctx -> do
    errOrLLVM <- runExceptT $ withModuleFromAST ctx m moduleLLVMAssembly
    case errOrLLVM of
      Left err -> putStrLn $ "error: " ++ err
      Right llvm -> putStrLn llvm

genProgram :: B.Program -> IO ()
genProgram _ = toLLVM (module_ "h")