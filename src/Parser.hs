{-# LANGUAGE NoMonomorphismRestriction #-}

module Parser where

import AST

import Control.Applicative
import Text.Trifecta

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

bin_expr sub ops = chainl1 sub bin_ops
  where bin_ops    = foldr1 (<|>) $ map (uncurry bin_op) ops
        bin_op s v = (RBinary ??) <$> (term s *> pure v)
        (??) f y x = f x y

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

rvalue = assign_expr

primary_expr = (RLVal <$> lvalue)
  <|> (RConstant <$> constant)
  <|> parens assign_expr

-- TODO: Support postfix ++, --
postfix_expr = try (RCall <$> primary_expr <*> parens (commaSep assign_expr))
  <|> primary_expr

unary_expr = try (RUnary <$> unary_op <*> unary_expr)
  <|> postfix_expr
  where unary_op = terms [("&", AddressOf), ("!", ArithNegate),
                          ("-", ArithNegate), ("++", Inc), ("--", Dec)]

mult_expr  = bin_expr unary_expr
  [("*", Multiply), ("/", Divide), ("%", Modulo)]
add_expr   = bin_expr mult_expr  [("+", Add), ("-", Subtract)]
shift_expr = bin_expr add_expr   [(">>", RightShift), ("<<", LeftShift)]
rel_expr   = bin_expr shift_expr
  [("<=", LessThanEqual), ("<", LessThan), (">=", GreaterThanEqual),
   (">", GreaterThan)]
eq_expr    = bin_expr rel_expr   [("==", Equal), ("!=", NotEqual)]
and_expr   = bin_expr eq_expr    [("&", And)]
or_expr    = bin_expr and_expr   [("|", Or)]
cond_expr = try (RTernary <$> or_expr <*> (term "?" *> assign_expr <* term ":")
                  <*> cond_expr)
  <|> or_expr

assign_expr = try (RAssign <$> lvalue <*> assign <*> assign_expr)
  <|> cond_expr
  where assign = toAssign <$> (string "=" *> optional binary) <* spaces
        toAssign Nothing   = Assign
        toAssign (Just op) = AssignWith op

binary = terms [("&", And), ("==", Equal), ("!=", NotEqual),
  ("<=", LessThanEqual), (">=", GreaterThanEqual), ("<<", LeftShift),
  (">>", RightShift), (">", GreaterThan), ("<", LessThan), ("-", Subtract),
  ("+", Add), ("%", Modulo), ("*", Multiply), ("/", Divide), ("|", Or)]

-- TODO: Support vector indexing
lvalue = (Name <$> name)
  <|> (Deref <$> (term "*" *> rvalue))
--  <|> (Index <$> rvalue <*> brackets rvalue)

constant = (NatLit <$> natural)
  <|> (CharLit <$> stringLiteral')
  <|> (StringLit <$> stringLiteral)

name = (alpha `before` many alpha_digit) <* spaces
  where alpha       = letter <|> char '_'
        alpha_digit = alpha <|> digit
        before p q  = (:) <$> p <*> q
