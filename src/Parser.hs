{-# LANGUAGE NoMonomorphismRestriction #-}

module Parser where

import AST

import Control.Applicative
import Text.Trifecta
import Text.Parser.Expression
import Text.Parser.Token
import Text.Parser.Token.Style

-- TODO: remove
import Text.Trifecta.Delta
parse :: Parser a -> String -> Result a
parse p s = parseString p (Columns 0 0) s

-- combinators
term :: CharParsing m => String -> m String
term s = string s <* spaces

termTo :: CharParsing m => String -> a -> m a
termTo s v = term s *> pure v

terms = foldr1 (<|>) . map (uncurry termTo)

-- nonterminals
program = Program <$> many definition

definition = try (Global <$> name <*> optional (brackets constant)
                <*> (commaSep ival <* semi))
  <|> Function <$> name <*> parens (commaSep name) <*> statement

ival = (IConstant <$> constant)
  <|> (IName <$> name)

statement = autos
  <|> (Externs <$> (term "extrn" *> commaSep1 name) <*> statement)
  <|> (Case <$> (term "case" *> constant <* colon) <*> statement)
  <|> (Statements <$> braces (many statement))
  <|> (If <$> (term "if" *> parens rvalue) <*> statement
        <*> optional (term "else" *> statement))
  <|> (While <$> (term "while" *> parens rvalue) <*> statement)
  <|> (Switch <$> (term "switch" *> rvalue) <*> statement)
  <|> (Goto <$> (term "goto" *> rvalue <* semi))
  <|> (Return <$> (term "return" *> optional rvalue <* semi))
  <|> try (StatementVal <$> optional rvalue <* semi)
  <|> (Label <$> (name <* colon) <*> statement)

autos = Auto <$> (term "auto" *> 
  (commaSep1 ((,) <$> name <*> optional constant)) <* semi) <*> statement

rvalue :: (Monad m, TokenParsing m) => m RValue
rvalue = assign_expr

primary_expr = (RLVal <$> lvalue)
  <|> (RConstant <$> constant)
  <|> parens assign_expr

postfix_expr = try (RCall <$> primary_expr <*> parens (commaSep assign_expr))
  <|> primary_expr

binop_expr :: (Monad m, TokenParsing m) => m RValue
binop_expr = buildExpressionParser binop_table postfix_expr

binop_table :: (Monad m, TokenParsing m) => [[Operator m RValue]]
binop_table = [ [unary "&" AddressOf, unary "!" ArithNegate, unary "-" ArithNegate, unary "++" Inc, unary "--" Dec, unaryp "++" Inc, unaryp "--" Dec]
              , [binary "*" Multiply, binary "/" Divide, binary "%" Modulo]
              , [binary "+" Add, binary "-" Subtract]
              , [binary ">>" RightShift, binary "<<" LeftShift]
              , [binary "<=" LessThanEqual, binary "<" LessThan,
                 binary ">=" GreaterThanEqual, binary ">" GreaterThan]
              , [binary "==" Equal, binary "!=" NotEqual]
              , [binary "&" And]
              , [binary "|" Or]
              ]

unary name op = Prefix $ RUnary op <$ reservedOp name
unaryp name op = Postfix $ RUnaryP op <$ reservedOp name

binary name op = Infix (fun <$ reservedOp name) AssocLeft
    where fun l r = RBinary l op r

reservedOp name = reserve emptyOps name

cond_expr = try (RTernary <$> binop_expr <*> (term "?" *> assign_expr <* term ":")
                  <*> cond_expr)
  <|> binop_expr

assign_expr = try (RAssign <$> lvalue <*> assign <*> assign_expr)
  <|> cond_expr
  where assign = toAssign <$> (string "=" *> optional binop) <* spaces
        toAssign Nothing   = Assign
        toAssign (Just op) = AssignWith op

binop = terms [("&", And), ("==", Equal), ("!=", NotEqual),
  ("<=", LessThanEqual), (">=", GreaterThanEqual), ("<<", LeftShift),
  (">>", RightShift), (">", GreaterThan), ("<", LessThan), ("-", Subtract),
  ("+", Add), ("%", Modulo), ("*", Multiply), ("/", Divide), ("|", Or)]

lvalue :: (Monad m, TokenParsing m) => m LValue
lvalue = combine <$> lvalue_h <*> many (brackets rvalue)
  where lvalue_h = (Name <$> name) <|> (Deref <$> (term "*" *> rvalue))
        combine h t = case t of
                          []     -> h
                          (x:xs) -> combine (Index (RLVal h) x) xs

constant = (NatLit <$> natural)
  <|> (CharLit <$> stringLiteral')
  <|> (StringLit <$> stringLiteral)

name = (alpha `before` many alpha_digit) <* spaces
  where alpha       = letter <|> char '_'
        alpha_digit = alpha <|> digit
        before p q  = (:) <$> p <*> q
