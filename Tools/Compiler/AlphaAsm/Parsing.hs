module Compiler.AlphaAsm.Parsing (
    parseAlpha, ParseResult
  ) where

import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>), many)
import Data.Char
import Compiler.AlphaAsm.Ast

import Debug.Trace

-- | Ein Alias für einen Operator, mit dem dieser als Zeichen verfügbar gemacht wird.
data OpAlias = OpAlias String Operator deriving (Show)

-- | Allgemeiner Typ für einen Alpha-Notations-Parser.
type AlphaParser a = GenParser Char () a

-- | Ergebnistyp des Parsers
type ParseResult = Either ParseError Program

operators :: [AlphaParser Operator]
operators = map makeOp [ OpAlias "+" Add, OpAlias "-" Sub, OpAlias "*" Mul
                       , OpAlias "/" Div, OpAlias "%" Mod
                       , OpAlias "=" Eq,  OpAlias "#" Ne,  OpAlias ">" Gt
                       , OpAlias ">=" Gt] where
  makeOp (OpAlias s o) = string s *> return o

------------ HILFSPARSER --------------------
parentheses :: AlphaParser a -> AlphaParser a
parentheses a = spaces *> char '(' *> spaces *> a <* spaces <* char ')' <* spaces

spaced :: AlphaParser a -> AlphaParser a
spaced a = spaces *> a <* spaces

------------ TERMINALE ------------------------

-- | Parst eine nicht-negative Ganzzahl.
nonNegative :: AlphaParser Int
nonNegative = foldl (\n d -> 10 * n + d) 0 <$> digits where
  digits = map digitToInt <$> many1 digit

-- | Parst eine Ganzzahl
int :: AlphaParser Int
int = ((char '-' >> return negate) <|> return id) <*> nonNegative

-- | Parst eine Register-Identifier.
registerIdent :: AlphaParser String
registerIdent = char '$' *> many1 letter

-- | Parst den Identifier einer symbolischen Adresse.
symbolicAddress :: AlphaParser String
symbolicAddress = (:) <$> letter <*> many alphaNum

labelIdent :: AlphaParser String
labelIdent = many1 upper

eos :: AlphaParser ()
eos = spaces *> char ';' *> spaces

------------ NICHT-TERMINALE -------------------

-- | Parst einen der erlaubten Operatoren.
operator :: AlphaParser Operator
operator = choice operators

-- | Parst ein Register.
register :: AlphaParser LValue
register = Accu <$> registerIdent

-- | Parst einen Speicherzugriff
memory :: AlphaParser LValue
memory = Mem <$> memAccess (indirect <|> direct)
  where
    memAccess x = char 'p' *> parentheses x
    direct = ADirect <$> addr
    indirect = AIndirect <$> memAccess addr
    addr = anum <|> asym <|> areg
    anum = AddrI <$> nonNegative
    asym = AddrS <$> symbolicAddress
    areg = AddrR <$> registerIdent

lvalue :: AlphaParser LValue
lvalue = memory <|> register

rvalue :: AlphaParser RValue
rvalue = (RVal <$> lvalue) <|> (RConst <$> int)

statement :: AlphaParser Statement
statement = foldl1 (<|>) [lbl, jmp, jnz
                         ,try mov, try mop
                         ,try push, try pop, sop] where
  mop = AssignOp <$> spaced lvalue <* string ":=" <*> spaced rvalue <*> operator <*> spaced rvalue <* eos
  mov = Assign <$> spaced lvalue <* string ":=" <*> spaced rvalue <* eos

  lbl = Label <$> labelIdent <* char ':' <* spaces
  jmp = Jump <$> (string "goto" *> spaced labelIdent <* eos)
  jnz = JumpNZ <$> (string "if" *> spaced rvalue) <*> operator <*> spaced rvalue
                <* string "then" <* spaces <* string "goto" <*> spaced labelIdent <* eos
  push = string "push" *> pure Push <* eos
  pop = string "pop" *> pure Pop <* eos
  sop = string "stack" *> pure StackOp <*> spaced operator <* eos

program :: AlphaParser Program
program = Program <$> many statement <* eof

parseAlpha :: SourceName -> String -> Either ParseError Program
parseAlpha = parse program