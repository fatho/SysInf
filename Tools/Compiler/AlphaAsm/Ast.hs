module Compiler.AlphaAsm.Ast (
    Address (..), AddressVal (..), LValue (..), RValue (..),
    Operator (..), Statement (..), Program (..),
    isComparison
  ) where

-- | Wert der Addresse. Möglich sind eine nicht-negative Ganzahl, ein
--   Register oder ein symbolischer Name.
data AddressVal = AddrI Int | AddrR String | AddrS String deriving (Show)

-- | Addressierung eines Elements im Speicher.
--   AddrI: Adressierung über numerische Adresse
--   AddrS: Adressiernung über symbolische Adresse
--   AddrR: Adressierung über Register
data Address = AIndirect AddressVal 
             | ADirect AddressVal deriving (Show)

-- | Ein L-Value ist entweder ein Akkumulator oder eine Speicherzelle
data LValue = Accu String 
            | Mem Address deriving (Show)

-- | Jeder L-Value ist auch ein R-Value. Außerdem konstante Werte.
data RValue = RVal LValue 
            | RConst Int deriving (Show)

-- | Die von der Alpha-Notation unterstützten Operatoren
data Operator = Add | Mul | Sub | Div | Mod 
              | Eq | Ne | Le | Lt | Ge | Gt deriving (Show, Eq, Bounded, Enum)

-- | Die Möglichen Arten von Statements.
data Statement = Assign LValue RValue
               | AssignOp LValue RValue Operator RValue
               | Label String
               | Jump String
               | JumpNZ RValue Operator RValue String
               | Push 
               | Pop 
               | StackOp Operator
               deriving (Show)

-- | Kapselt ein Alpha-Notations-Programm.
newtype Program = Program [Statement] deriving (Show)

-- | Prädikat, das testet, ob es sich um einen Vergleichsoperator handelt
isComparison :: Operator -> Bool
isComparison op = op `elem` [Eq, Ne, Le, Lt, Ge, Gt]