{-# LANGUAGE NoMonomorphismRestriction #-}

module Parser (parse, constant, program) where

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

terms :: CharParsing m => [String] -> m String
terms = choice . map term

-- nonterminals
-- TODO:
--  * remove left recursion
--  * split out terminals/nonterminals
--  * split out large nonterminals into smaller functions
program = many definition

definition = (name *> optional (brackets constant) *> (commaSep ival) *> semi) <|>
  (name *> parens (optional $ commaSep name) *> statement)

ival = constant <|> name

statement = (term "auto" *> (commaSep1 $ name *> optional constant) *> semi *> statement) <|>
  (term "extrn" *> (commaSep1 name) *> semi *> statement) <|>
  (name *> colon *> statement) <|>
  (term "case" *> constant *> colon *> statement) <|>
  ((braces $ many statement) *> pure 'x') <|>
  (term "if" *> parens rvalue *> statement *> optional (term "else" *> statement) *> pure 'x') <|>
  (term "while" *> parens rvalue *> statement) <|>
  (term "switch" *> rvalue *> statement) <|>
  (term "goto" *> rvalue *> semi) <|>
  (term "return" *> optional rvalue *> semi) <|>
  (optional rvalue *> semi)

rvalue = (parens rvalue) <|>
  lvalue <|>
  constant <|>
  (lvalue *> assign *> rvalue) <|>
  (inc_dec *> lvalue) <|>
  (lvalue *> inc_dec) <|>
  (unary *> rvalue) <|>
  (term "&" *> rvalue) <|>
  (rvalue *> binary *> rvalue) <|>
  (rvalue *> term "?" *> rvalue *> colon *> rvalue) <|>
  (rvalue *> parens (optional $ commaSep rvalue) *> pure "")

assign = term "=" *> optional binary

inc_dec = terms ["++", "--"]

unary = terms ["-", "!"]

binary = terms ["&", "==", "!=", "<", "<=", ">", ">=", "<<", ">>", "-", "+",
  "%", "*", "/"]

lvalue = name <|>
  (term "*" *> rvalue) <|>
  (rvalue *> brackets rvalue)

constant =
  (natural *> pure "") <|>
  stringLiteral' <|>
  stringLiteral

name = alpha `before` many alpha_digit
  where alpha       = letter <|> char '_'
        alpha_digit = alpha <|> digit
