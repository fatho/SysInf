module Compiler.SPOZI where

import Control.Monad
import Data.Maybe
import qualified Data.Map as Map

-- Typen zur Programmbeschreibung
data Source = Reg Int | Val Integer deriving (Show, Eq)
type Instruction = (Source, Source, Int, Int)
type Program = [Instruction]

-- Typen für das Runtimesystem
type Memory = Map.Map Int Integer
data ProgramState = PS { prog :: Program
                       , memory :: Memory
                       , programCounter :: Int } deriving (Show, Eq)
data Err = Err String deriving (Show, Eq)
type RunResult = Either Err ProgramState

initialState :: Program -> [(Int,Integer)] -> ProgramState
initialState prg mem = PS { prog = prg, memory = Map.fromList mem, programCounter = 1 }

-- Ließt ein Register aus dem Programmzustand aus
readReg :: Int -> ProgramState -> Either Err Integer
readReg i ps = readMem i (memory ps) where
  readMem :: Int -> Memory -> Either Err Integer
  readMem i mem = case Map.lookup i mem of
    Nothing  -> Left $ Err "read operation from uninitialized register"
    Just val -> Right val

-- Verändert ein Register und gibt den neuen Programmstatus zurück
writeReg :: Int -> Integer -> ProgramState -> RunResult
writeReg i val ps = 
  do newMem <- writeMem i val (memory ps)
     return $ ps { memory = newMem }
  where
    writeMem :: Int -> Integer -> Memory -> Either Err Memory
    writeMem i val mem | i < 1 || i > 6 = Left $ Err "invalid register"
                       | otherwise = Right $ Map.insert i val mem


getVal :: Source -> ProgramState -> Either Err Integer
getVal (Val i) _ = return i
getVal (Reg r) p = readReg r p


modifyPC :: (Int -> Int) -> ProgramState -> ProgramState
modifyPC f ps = let pc = programCounter ps
                 in ps { programCounter = f pc }


executeStep :: ProgramState -> RunResult
executeStep state = 
    if finished 
      then Right state
      else do let (s1, s2, d, t) = instruction
              p <- getVal s1 state
              q <- getVal s2 state
              let result = p - q
              newState <- writeReg d result state
              let step = modifyPC $ if result < 0 then const t else (+1)
              executeStep $ step newState
  where
    finished = let pc = (programCounter state) 
                in pc < 1 || pc > (length (prog state))
    instruction = (prog state)!!(programCounter state - 1)

executeProgram :: Program -> [(Int,Integer)] -> RunResult
executeProgram p mem = executeStep $ initialState p mem

-- Beispielprogramme
afg4a :: Program
afg4a = [(Val 0, Reg 2, 3, 2)
        ,(Reg 1, Reg 3, 3, 3)]

afg4c :: Program
afg4c = [(Val 0, Reg 1, 4, 4)
        ,(Val 0, Reg 1, 1, 3)
        ,(Val 0, Reg 2, 2, 4)
        ,(Val 0, Reg 2, 2, 5)
        ,(Val 0, Val 0, 3, 6)
        ,(Reg 1, Val 1, 1, 9)
        ,(Reg 3, Reg 2, 3, 8)
        ,(Val 0, Val 1, 4, 6)]

