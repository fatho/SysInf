import Compiler.AlphaAsm
import Compiler.AlphaAsm.Ast
import Compiler.AlphaAsm.Parsing
import Compiler.AlphaAsm.Interpreter

import Data.List
import qualified Data.Map as M

import Control.Applicative
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Identity

import qualified System.IO.Strict as SIO

filename = "alpha.an"

parseTest = do  Right prg <- parseAlpha filename <$> SIO.readFile filename
                return $ runProgram prg (M.fromList [(0,2)]) M.empty M.empty

