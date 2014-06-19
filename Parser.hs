{-# LANGUAGE NoMonomorphismRestriction #-}

module Parser (parse, program) where

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


-- nonterminals
-- TODO:
--  * remove left recursion
--  * split out terminals/nonterminals
--  * allow preceding spaces in terminals
--  * split out large nonterminals into smaller functions
program = many definition

definition = (name *> optional (char '[' *> optional constant *> char ']') *> (commaSep ival) *> char ';') <|>
  (name *> char '(' *> optional (commaSep name) *> char ')' *> statement)

ival = constant <|> name

statement = (string "auto" *> (commaSep1 $ name *> optional constant) *> char ';' *> statement) <|>
  (string "extrn" *> (commaSep1 name) *> char ';' *> statement) <|>
  (name *> char ':' *> statement) <|>
  (string "case" *> constant *> char ':' *> statement) <|>
  (char '{' *> many statement *> char '}') <|>
  (string "if" *> char '(' *> rvalue *> char ')' *> statement *> optional (string "else" *> statement) *> pure 'x') <|>
  (string "while" *> char '(' *> rvalue *> char ')' *> statement) <|>
  (string "switch" *> rvalue *> statement) <|>
  (string "goto" *> rvalue *> char ';') <|>
  (string "return" *> optional rvalue *> char ';') <|>
  (optional rvalue *> char ';')

rvalue = (char '(' *> rvalue <* char ')') <|>
  lvalue <|>
  constant <|>
  (lvalue *> assign *> rvalue) <|>
  (inc_dec *> lvalue) <|>
  (lvalue *> inc_dec) <|>
  (unary *> rvalue) <|>
  (char '&' *> rvalue) <|>
  (rvalue *> binary *> rvalue) <|>
  (rvalue *> char '?' *> rvalue *> char ':' *> rvalue) <|>
  (rvalue *> char '(' *> optional (commaSep rvalue) *> pure "")

assign = optional binary

inc_dec = string "++" <|> string "--"

unary = char '-' <|> char '!'

binary = spaces *> choice bin_ops
  where bin_ops = map string ["&", "==", "!=", "<", "<=", ">", ">=", "<<",
                              ">>", "-", "+", "%", "*", "/"]

lvalue = name <|>
  (char '*' *> rvalue) <|>
  (rvalue *> char '[' *> rvalue *> char ']' *> pure "")

constant =
  (digit `before` many digit)   <|>
  (squote *> mnOf (noneOf "'") 1 2 <* squote) <|>
  (dquote *> many (noneOf "\"") <* dquote)
 where squote = char '\''
       dquote = char '"'

name = alpha `before` many alpha_digit

alpha_digit = alpha <|> digit

alpha = letter <|> char '_'
