{-# LANGUAGE TemplateHaskell,TypeSynonymInstances,FlexibleContexts #-}

module Compiler.AlphaAsm.Interpreter (
    ErrorMsg, Memory, Registers, Symbols,
    runProgram
  )
  where

import Compiler.AlphaAsm.Ast

import Prelude hiding ((.), id)

import Data.Label hiding (get)
import qualified Data.Label as L
import Data.Label.PureM

import Control.Category
import Control.Monad
import Control.Monad.State hiding (modify, gets)
import Control.Monad.Trans.Either
import Control.Monad.Identity
import Control.Error.Util
import Control.Applicative

import qualified Data.Map as M
import Data.Maybe

type StateWithErrorT s e m = EitherT e (StateT s m)

type InterpreterT m = StateWithErrorT IntState ErrorMsg m
type Interpreter = InterpreterT Identity
type ErrorMsg = String

type Memory = M.Map Int Int
type Registers = M.Map String Int
type Symbols = M.Map String Int

runStateErrorT :: StateWithErrorT s e m a -> s -> m (Either e a, s)
runStateErrorT m = runStateT (runEitherT m)

runInterpreterT :: InterpreterT m a -> IntState -> m (Either ErrorMsg a, IntState)
runInterpreterT = runStateErrorT

runInterpreter :: Interpreter a -> IntState -> (Either ErrorMsg a, IntState)
runInterpreter i = runIdentity . runInterpreterT i

data IntState = IntState {
    _program :: Program,
    _memory :: Memory,
    _registers :: Registers,
    _symbols :: Symbols,
    _labels :: M.Map String Int,
    _progCounter :: Int,
    _stack :: [Int]
  } deriving (Show)

mkLabels [''IntState]

handleMaybe :: String -> Maybe a -> Interpreter a
handleMaybe str = noteT str . hoistMaybe

getOp :: Operator -> Int -> Int -> Int
getOp o = case o of
  Add -> (+)
  Sub -> (-)
  Mul -> (*)
  Div -> div
  Mod -> mod
  Eq  -> bint.(==)
  Ne  -> bint.(/=)
  Le  -> bint.(<=)
  Lt  -> bint.(<)
  Ge  -> bint.(>=)
  Gt  -> bint.(>)
  where bint b = bint' . b
        bint' b = if b then 1 else 0


----------- SPEICHER- UND REGISTERZUGRIFFE ------------------------------

readGen :: (Ord k, MonadState a Interpreter) => (:->) a (M.Map k v) -> k -> Interpreter (Maybe v)
readGen l x = M.lookup x <$> gets l

writeGen :: (Ord k, MonadState a Interpreter) => (:->) a (M.Map k v) -> k -> v -> Interpreter ()
writeGen l k v = l =. M.insert k v

readReg :: String -> Interpreter Int
readReg r = readGen registers r >>= handleMaybe ("read from unintialized register " ++ r)

readSym :: String -> Interpreter Int
readSym s = readGen symbols s >>= handleMaybe ("unknown symbol " ++ s)

readLbl :: String -> Interpreter Int
readLbl s = readGen labels s >>= handleMaybe ("unknown label " ++ s)

readMem :: Address -> Interpreter Int
readMem (ADirect a) = readMemDirect a
readMem (AIndirect a) = readMemDirect a >>= readMemDirect.AddrI

writeMem :: Address -> Int -> Interpreter ()
writeMem (ADirect a) v = writeMemDirect v a
writeMem (AIndirect a) v = readMemDirect a >>= writeMemDirect v . AddrI

writeMemDirect :: Int -> AddressVal -> Interpreter ()
writeMemDirect v (AddrI i) = writeGen memory i v
writeMemDirect v (AddrS s) = readSym s >>= writeMemDirect v . AddrI
writeMemDirect v (AddrR r) = readReg r >>= writeMemDirect v . AddrI

readMemDirect :: AddressVal -> Interpreter Int
readMemDirect (AddrI i) = readGen memory i >>= handleMaybe ("read from unintialized memory at " ++ show i)
readMemDirect (AddrS s) = readSym s >>= readMemDirect.AddrI
readMemDirect (AddrR r) = readReg r >>= readMemDirect.AddrI

readLValue :: LValue -> Interpreter Int
readLValue (Accu r) = readReg r
readLValue (Mem a) = readMem a

readRValue :: RValue -> Interpreter Int
readRValue (RVal l) = readLValue l
readRValue (RConst c) = return c

performOp :: RValue -> Operator -> RValue -> Interpreter Int
performOp r1 op r2 = getOp op <$> readRValue r1 <*> readRValue r2

assign :: LValue -> RValue -> Interpreter ()
assign (Accu r) v = readRValue v >>= writeGen registers r
assign (Mem r) v = readRValue v >>= writeMem r

assignOp :: LValue -> RValue -> Operator -> RValue -> Interpreter ()
assignOp a l o r = performOp l o r >>= assign a . RConst

jump :: String -> Interpreter ()
jump lbl = readLbl lbl >>= (=:) progCounter

jumpIf :: RValue -> Operator -> RValue -> String -> Interpreter ()
jumpIf l o r lbl = performOp l o r >>= \i -> when (i /= 0) $ jump lbl

push :: Int -> Interpreter ()
push i = stack =. (i:)

pop :: Interpreter Int
pop = gets stack >>= \t -> case t of 
  [] -> left "cannot pop, stack is empty"
  (x:xs) -> stack =: xs >> writeGen registers "a" x >> return x

stackOp :: Operator -> Interpreter ()
stackOp op = getOp op <$> pop <*> pop >>= push

doStatement :: Statement -> Interpreter ()
doStatement stmt = case stmt of 
  Assign a r       -> assign a r
  AssignOp a l o r -> assignOp a l o r
  Jump lbl         -> jump lbl
  JumpNZ l o r lbl -> jumpIf l o r lbl
  Push             -> push =<< readReg "a"
  Pop              -> void pop
  StackOp o        -> stackOp o
  _                -> return ()

finished :: Interpreter Bool
finished = do Program p <- gets program
              pc        <- gets progCounter
              return $ pc < 0 || pc >= length p

step :: Interpreter ()
step = do Program p <- gets program
          pc        <- gets progCounter
          let stmt = p !! pc
          progCounter =. (+1)
          doStatement stmt

setupState :: Program -> Memory -> Registers -> Symbols -> IntState
setupState p mem reg sym = IntState {
  _program = p, _memory = mem, _registers = reg,
  _symbols = sym, _labels = lbls, _progCounter = begin, _stack = [] } 
  where
    stmts = let (Program s) = p in s
    begin = fromJust (M.lookup "ENTRY" lbls <|> Just 0)
    lbls = M.fromList $ map (\(Label l,v) -> (l,v)) $ filter isLabel $ zip stmts [1..]
    isLabel (l,_) = case l of 
      Label _ -> True
      _       -> False

runProgram :: Program -> Memory -> Registers -> Symbols -> Either ErrorMsg (Memory,Registers)
runProgram p mem reg sym = extract <$> env where
  extract s = (L.get memory s, L.get registers s)
  env = fst $ runInterpreter run (setupState p mem reg sym)
  run = finished >>= \f -> if f then lift get else step >> run