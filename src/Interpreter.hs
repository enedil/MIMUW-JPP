module Interpreter where

import Control.Monad.Reader --(ReaderT, ask, runReader, local)
import Control.Monad.Identity
import Control.Monad.Trans.Identity
--import Control.Monad.Trans.Reader
import Control.Monad.Except
--import qualified Data.Map as M
import Debug.Trace

import qualified AbsCerber as S

type IsInLoop = Bool
type Pos = S.BNFC'Position

chck :: S.Program -> Except Pos ()
chck (S.PProgram _ fns) = 
    check_is_in_loop_independent [(pos, trace ("aaa " ++ (show body) ++ "\n") body) | (S.FnDef pos _ _ _ body) <- fns]

-- TODO: lambdy z break/continue

check_is_in_loop_independent :: [(Pos, S.Block)] -> Except Pos ()
check_is_in_loop_independent [] = return ()
check_is_in_loop_independent ((pos, body):b) = do
    runReaderT (check_is_in_loop (S.BStmt pos body)) False
    check_is_in_loop_independent b

check_is_in_loop :: S.Stmt -> ReaderT IsInLoop (Except Pos) ()
check_is_in_loop (S.While _ _ stmt1) = do
    local (const True) $ check_is_in_loop stmt1
check_is_in_loop (S.ForIn _ _ _ stmt2) = do
    local (const True) $ check_is_in_loop stmt2
check_is_in_loop (S.Continue pos) = do
    in_loop <- ask
    if in_loop then return () else throwError pos
check_is_in_loop (S.Break pos) = do
    in_loop <- ask
    if in_loop then return () else throwError pos
check_is_in_loop (S.BStmt _ (S.BBlock _ stmts)) = do
    sequence_ (map check_is_in_loop stmts) >>= return
check_is_in_loop _ = return ()
--check_is_in_loop (S.
