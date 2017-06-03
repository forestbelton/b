module AST where

data Program = Program [Definition]
  deriving (Show)

data Definition = Global String (Maybe Constant) [IVal]
  | Function String [String] Statement
  deriving (Show)

data IVal = IConstant Constant
  | IName String
  deriving (Show)

data Statement = Auto [(String, Maybe Constant)] Statement
  | Externs [String] Statement
  | Label String Statement
  | Case Constant Statement
  | Statements [Statement]
  | If RValue Statement (Maybe Statement)
  | While RValue Statement
  | Switch RValue Statement
  | Goto RValue
  | Return (Maybe RValue)
  | StatementVal (Maybe RValue)
  deriving (Show)

data RValue = RLVal LValue
  | RConstant Constant
  | RAssign LValue AssignOp RValue
  | RUnary UnaryOp RValue
  | RUnaryP UnaryOp RValue
  | RBinary RValue BinOp RValue
  | RTernary RValue RValue RValue
  | RCall RValue [RValue]
  deriving (Show)

data AssignOp = Assign | AssignWith BinOp
  deriving (Show)

data UnaryOp = ArithNegate | LogicNegate | AddressOf | Inc | Dec
  deriving (Show)

data BinOp = And | Equal | NotEqual | LessThan | LessThanEqual | GreaterThan
 | GreaterThanEqual | LeftShift | RightShift | Subtract | Add | Modulo
 | Multiply | Divide | Or
  deriving (Show)

data LValue = Name String | Deref RValue | Index RValue RValue
  deriving (Show)

data Constant = CharLit String | StringLit String | NatLit Integer
  deriving (Show)
