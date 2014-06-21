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
mnOf :: CharParsing m => m a -> Int -> Int -> m [a]
mnOf p m n = undefined -- TODO

before :: CharParsing m => m a -> m [a] -> m [a]
before p q = (:) <$> p <*> q

term :: CharParsing m => String -> m String
term s = string s <* spaces

termTo :: CharParsing m => String -> a -> m a
termTo s v = term s *> pure v

terms :: CharParsing m => [String] -> m String
terms = choice . map term

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

-- TODO: Remove left recursion here
rvalue = (RConstant <$> constant)
  <|> try (RAssign <$> lvalue <*> assign <*> rvalue)
  <|> (RPreIncDec <$> inc_dec <*> lvalue)
  <|> try (RPostIncDec <$> lvalue <*> inc_dec)
  <|> (RUnary <$> unary <*> rvalue)
  <|> (RLVal <$> lvalue)
--  <|> (RBinary <$> rvalue <*> binary <*> rvalue)
--  <|> (RTernary <$> rvalue <*> rvalue <*> rvalue)
--  <|> (RCall <$> rvalue <*> parens (commaSep rvalue))

assign = toAssign <$> (string "=" *> optional binary)
  where toAssign Nothing   = Assign
        toAssign (Just op) = AssignWith op

inc_dec = (termTo "++" Inc)
  <|> (termTo "--" Dec)

unary = (termTo "-" ArithNegate)
  <|> (termTo "!" LogicNegate)
  <|> (termTo "&" AddressOf)

binary = (termTo "&" And)
  <|> (termTo "==" Equal)
  <|> (termTo "!=" NotEqual)
  <|> try (termTo "<=" LessThanEqual)
  <|> try (termTo ">=" GreaterThanEqual)
  <|> try (termTo "<<" LeftShift)
  <|> try (termTo ">>" RightShift)
  <|> (termTo ">"  GreaterThan)
  <|> (termTo "<"  LessThan)
  <|> (termTo "-"  Subtract)
  <|> (termTo "+"  Add)
  <|> (termTo "%"  Modulo)
  <|> (termTo "*"  Multiply)
  <|> (termTo "/"  Divide)

-- TODO: Remove left recursion here
lvalue = (Name <$> name)
  <|> (Deref <$> (term "*" *> rvalue))
--  <|> (Index <$> rvalue <*> brackets rvalue)

constant = (NatLit <$> natural)
  <|> (CharLit <$> stringLiteral')
  <|> (StringLit <$> stringLiteral)

name = (alpha `before` many alpha_digit) <* spaces
  where alpha       = letter <|> char '_'
        alpha_digit = alpha <|> digit
