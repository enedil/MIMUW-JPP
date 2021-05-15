{-# LANGUAGE RankNTypes #-}
module TypeChecker where

import Control.Monad.Reader --(ReaderT, ask, runReader, local)
--import Control.Monad.Identity
--import Data.Maybe
--import Control.Monad.Trans.Identity
import Control.Monad.Trans.Except
--import Control.Monad.Trans.Reader
import Control.Monad.Except
import qualified Data.Map as M
--import Text.Printf (printf)
import Debug.Trace

import qualified AbsCerber as S

type Pos = S.BNFC'Position

returnRegister :: String
returnRegister = "return"
type TypeEnv = M.Map String S.Type

type TypeCheckerMonad a = ExceptT String (Reader TypeEnv) a

data Type = TInt | TStr | TBool | TVoid | Fun Type [Type] | Tuple [Type]

formatError :: forall a. Pos -> String -> TypeCheckerMonad a
formatError (Just (line, col)) arg = throwE $ (show line) ++ ":" ++ (show col) ++ ": " ++ arg
formatError Nothing arg = throwE $ "(unknown): " ++ arg

cmptypes :: S.Type -> S.Type -> Pos -> String -> TypeCheckerMonad ()
cmptypes t1 t2 pos msg = if t1 == t2 then return () else formatError pos msg

typeShow :: S.Type -> String
typeShow = show

typeOf :: S.Expr -> TypeEnv -> Either String S.Type
typeOf e initEnv = let ty = runReader (runExceptT (typeOf_ e)) initEnv in (trace ("expr=" ++ (show e) ++ ", type=" ++ (show ty)) ty)

typeOf_ :: S.Expr -> TypeCheckerMonad S.Type
typeOf_ e@(S.EVar _ (S.Ident name)) = do
    type_ <- asks (M.lookup name)
    case type_ of
        Just t -> return t
        Nothing -> formatError (S.hasPosition e) ("Variable or function not in scope: " ++ name)

typeOf_ e@(S.ELitInt _ _) = return $ S.Int (S.hasPosition e)
typeOf_ e@(S.ELitTrue _) = return $ S.Bool (S.hasPosition e)
typeOf_ e@(S.ELitFalse _) = return $ S.Bool (S.hasPosition e)
typeOf_ e@(S.EString _ _) = return $ S.Str (S.hasPosition e)
typeOf_ e@(S.Neg _ e_in) = do
    type_in <- typeOf_ e_in
    case type_in of
        S.Int _ -> return $ S.Int (S.hasPosition e)
        _ -> formatError (S.hasPosition e) ("cannot negate: " ++ (typeShow type_in))
typeOf_ e@(S.Not _ e_in) = do
    type_in <- typeOf_ e_in
    case type_in of
        S.Bool _ -> return $ S.Bool (S.hasPosition e)
        _ -> formatError (S.hasPosition e) ("cannot invert: " ++ (typeShow type_in))
typeOf_ e@(S.EMul _ e1 _ e2) = do
    type1 <- typeOf_ e1
    type2 <- typeOf_ e2
    case (type1, type2) of
        (S.Int _, S.Int _) -> return $ S.Int (S.hasPosition e)
        _ -> formatError (S.hasPosition e) ("cannot multiply/divide: " ++ (typeShow type1) ++ " and " ++ (typeShow type2))
typeOf_ e@(S.EAdd _ e1 _ e2) = do
    type1 <- typeOf_ e1
    type2 <- typeOf_ e2
    case (type1, type2) of
        (S.Int _, S.Int _) -> return $ S.Int (S.hasPosition e)
        _ -> formatError (S.hasPosition e) ("cannot add/subtract: " ++ (typeShow type1) ++ " and " ++ (typeShow type2))
typeOf_ e@(S.ERel _ e1 _ e2) = do
    type1 <- typeOf_ e1
    type2 <- typeOf_ e2
    case (type1, type2) of
        (S.Int _, S.Int _) -> return $ S.Bool (S.hasPosition e)
        _ -> formatError (S.hasPosition e) ("cannot compare: " ++ (typeShow type1) ++ " and " ++ (typeShow type2))
typeOf_ e@(S.EAnd _ e1 e2) = do
    type1 <- typeOf_ e1
    type2 <- typeOf_ e2
    case (type1, type2) of
        (S.Bool _, S.Bool _) -> return $ S.Bool (S.hasPosition e)
        _ -> formatError (S.hasPosition e) ("cannot and: " ++ (typeShow type1) ++ " and " ++ (typeShow type2))
typeOf_ e@(S.EOr _ e1 e2) = do
    type1 <- typeOf_ e1
    type2 <- typeOf_ e2
    case (type1, type2) of
        (S.Bool _, S.Bool _) -> return $ S.Bool (S.hasPosition e)
        _ -> formatError (S.hasPosition e) ("cannot or: " ++ (typeShow type1) ++ " and " ++ (typeShow type2))
typeOf_ e@(S.ETuple _ exprs) = do
    types <- mapM typeOf_ exprs
    return $ S.Tuple (S.hasPosition e) types
typeOf_ e@(S.EApp _ fun args) = do
    fun_type <- typeOf_ fun 
    real_arg_types <- mapM typeOf_ args
    case fun_type of
        S.Fun _ ret_type arg_types -> 
            if real_arg_types == arg_types 
            then return $ ret_type 
            else formatError (S.hasPosition e) ("cannot call: " ++ (show fun) ++ " with " ++ (concat (map typeShow real_arg_types)))
        _ -> formatError (S.hasPosition e) ("not callable:" ++ (show fun))

typeCheckStmt :: S.Stmt -> TypeCheckerMonad ()
typeCheckStmt (S.BStmt _ (S.BBlock _ [])) = return ()
typeCheckStmt st@(S.BStmt _ (S.BBlock p (a:b))) = typeCheckStmt a >> typeCheckStmt (S.BStmt (S.hasPosition st) (S.BBlock p b))
typeCheckStmt st@(S.Ass _ (S.Ident ident) expr) = do
    env <- ask
    case (M.lookup ident env, typeOf expr env) of
        (Just id_t, Right expr_t) -> 
            cmptypes id_t expr_t (S.hasPosition st) $ "can't assign type " ++ (typeShow expr_t) ++ " to variable of type " ++ (typeShow id_t) 
        (Nothing, Right _) -> formatError (S.hasPosition st) $ "undefined variable " ++ ident
        (_, Left err) -> formatError (S.hasPosition st) err
typeCheckStmt st@(S.VRet _ ) = do
    env <- ask
    case M.lookup returnRegister env of
        Just (S.Void _) -> return ()
        _ -> formatError (S.hasPosition st) $ "cannot return void from nonvoid function"
typeCheckStmt st@(S.Ret _ e) = do
    env <- ask
    case (M.lookup returnRegister env, typeOf e env) of
        (Nothing, _) -> formatError (S.hasPosition st) " fatal error: return register not allocated"
        (Just t, Right e_t) -> 
            cmptypes t e_t (S.hasPosition st) $ "Cannot return expression of type " ++ (show e_t) ++ " from function with type " ++ (show t)
        (_, Left err) -> formatError (S.hasPosition st) err


checktype :: S.Type -> S.Type ->

typeCheckStmt st@(S.Cond _ e s) = do
    env <- ask
    (case typeOf e env of
        Right (S.Bool _) -> return ()
        Right _ -> formatError (S.hasPosition st) "condition is not bool"
        Left err-> formatError (S.hasPosition st) err) >> typeCheckStmt s

typeCheckStmt st@(S.CondElse _ e s1 s2) = do
    env <- ask
    (case typeOf e env of
        Right (S.Bool _) -> return ()
        Right _ -> formatError (S.hasPosition st) "condition is not bool"
        Left err-> formatError (S.hasPosition st) err) >> typeCheckStmt s1 >> typeCheckStmt s2

typeCheckStmt st@(S.SExp _ e) = do
    env <- ask
    case typeOf e env of
        Right _ -> return ()
        Left err-> formatError (S.hasPosition st) err
        
typeCheckStmt st@(S.While _ e s) = do
    env <- ask
    (case typeOf e env of
        Right (S.Bool _) -> return ()
        Right _ -> formatError (S.hasPosition st) "condition is not bool"
        Left err -> formatError (S.hasPosition st) err) >> typeCheckStmt s

typeCheckStmt st@(S.While _ e s) = do
    env <- ask
    (case typeOf e env of
        Right (S.Bool _) -> return ()
        Right _ -> formatError (S.hasPosition st) "condition is not bool"
        Left err -> formatError (S.hasPosition st) err) >> typeCheckStmt s

typeCheckStmt st@(S.ForIn _ ident e s) = do
    env <- ask
    (case typeOf e env of
        
    
--typeCheckStmt _ = return ()

typeCheckProgram :: S.Program -> Except String ()
typeCheckProgram prog = undefined

chck :: S.Program -> Except Pos ()
chck (S.PProgram _ fns) = 
    check_is_in_loop_independent [(pos, trace ("\nAaa " ++ (show body) ++ "\n") body) | (S.FnDef pos _ _ _ body) <- fns]

-- TODO: lambdy z break/continue


type IsInLoop = Bool
check_is_in_loop_independent :: [(Pos, S.Block)] -> Except Pos ()
check_is_in_loop_independent [] = return ()
check_is_in_loop_independent ((pos, body):b) = do
    runReaderT (check_is_in_loop (S.BStmt pos body)) False
    check_is_in_loop_independent b

check_is_in_loop :: S.Stmt -> ReaderT IsInLoop (Except Pos) ()
check_is_in_loop (S.While _ e stmt) = do
    local (const True) $ (trace ("xd:" ++ show (typeOf e M.empty) ++ "\n\n") check_is_in_loop) stmt
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
check_is_in_loop (S.SExp _ e) = return $ trace ("expr:" ++ (show e) ++ "\n") ()
check_is_in_loop _ = return ()
