{-# LANGUAGE RankNTypes #-}
module TypeChecker where

import Control.Monad.Reader --(ReaderT, ask, runReader, local)
--import Control.Monad.Identity
--import Data.Maybe
--import Control.Monad.Trans.Identity
import Data.Functor.Identity
import Control.Monad.Trans.Except
--import Control.Monad.Trans.Reader
import Control.Monad.Except
import Control.Monad.Fail
import qualified Data.Map as M
--import Text.Printf (printf)
import Debug.Trace

import qualified AbsCerber as S

type Pos = S.BNFC'Position

returnRegister :: String
returnRegister = "return"
type TypeEnv = M.Map String Type

type TypeCheckerMonad a = ExceptT String (Reader TypeEnv) a

data Type = TInt | TStr | TBool | TVoid | TFun Type [Type] | TTuple [Type] | TRef Type deriving (Eq, Ord)
instance Show Type where
    show = typeShow

typeOfStype :: S.Type -> Type
typeOfStype (S.Int _) = TInt
typeOfStype (S.Str _) = TStr
typeOfStype (S.Bool _) = TBool
typeOfStype (S.Void _) = TVoid
typeOfStype (S.Fun _ ret args) = TFun (typeOfStype ret) (map typeOfStype args)
typeOfStype (S.Function _ (ret:args)) = TFun (typeOfStype ret) (map typeOfStype args)
typeOfStype (S.Tuple _ elems) = TTuple (map typeOfStype elems)
typeOfStype x = (trace (show x) undefined)

formatError :: forall a. Pos -> String -> TypeCheckerMonad a
formatError (Just (line, col)) arg = throwE $ (show line) ++ ":" ++ (show col) ++ ": " ++ arg
formatError Nothing arg = throwE $ "(unknown): " ++ arg

cmptypes :: Type -> Type -> Pos -> String -> TypeCheckerMonad ()
cmptypes t1 t2 pos msg = if t1 == t2 then return () else formatError pos msg

typeShow :: Type -> String
typeShow TInt = "int"
typeShow TStr = "str"
typeShow TBool = "bool"
typeShow TVoid = "void"
typeShow (TFun ret args) = "function<" ++ (typeShow ret) ++ (show (map typeShow args)) ++ ">"
typeShow (TTuple elems) = "tuple<" ++ (show (map typeShow elems)) ++ ">"

typeOf :: S.Expr -> TypeEnv -> Either String Type
typeOf e initEnv = let ty = runReader (runExceptT (typeOf_ e)) initEnv in (trace ("expr=" ++ (show e) ++ ", type=" ++ (show ty)) ty)

expecttype :: Type -> Type -> Type -> Pos -> String -> TypeCheckerMonad Type
expecttype expected input rettype pos err = if expected == input then return rettype else formatError pos err
expecttype2 :: Type -> Type -> Type -> Type -> Pos -> String -> TypeCheckerMonad Type
expecttype2 expected input1 input2 rettype pos err = if expected == input1 && expected == input2 then return rettype else formatError pos err

typeOf' :: S.Expr -> TypeCheckerMonad Type
typeOf' x = trace ("expr=" ++ (show x) ++ "\n") $ typeOf_ x

typeOf_ :: S.Expr -> TypeCheckerMonad Type
typeOf_ e@(S.EVar _ (S.Ident name)) = do
    type_ <- asks (M.lookup name)
    case type_ of
        Just t -> return t
        Nothing -> formatError (S.hasPosition e) ("Variable or function not in scope: " ++ name)

typeOf_ e@(S.ELitInt _ _) = return TInt
typeOf_ e@(S.ELitTrue _) = return TBool 
typeOf_ e@(S.ELitFalse _) = return TBool
typeOf_ e@(S.EString _ _) = return TStr
typeOf_ e@(S.Neg _ e_in) = do
    type_in <- typeOf' e_in
    expecttype TInt type_in TInt (S.hasPosition e) ("cannot negate: " ++ (typeShow type_in))
typeOf_ e@(S.Not _ e_in) = do
    type_in <- typeOf' e_in
    expecttype TInt type_in TInt (S.hasPosition e) ("cannot invert: " ++ (typeShow type_in))
typeOf_ e@(S.EMul _ e1 _ e2) = do
    type1 <- typeOf' e1
    type2 <- typeOf' e2
    expecttype2 TInt type1 type2 TInt (S.hasPosition e) ("cannot multiply/divide: " ++ (typeShow type1) ++ " and " ++ (typeShow type2))
typeOf_ e@(S.EAdd _ e1 _ e2) = do
    type1 <- typeOf' e1
    type2 <- typeOf' e2
    expecttype2 TInt type1 type2 TInt (S.hasPosition e) ("cannot add/subtract: " ++ (typeShow type1) ++ " and " ++ (typeShow type2))
typeOf_ e@(S.ERel _ e1 _ e2) = do
    type1 <- typeOf' e1
    type2 <- typeOf' e2
    expecttype2 TInt type1 type2 TBool (S.hasPosition e) ("cannot compare: " ++ (typeShow type1) ++ " and " ++ (typeShow type2))
typeOf_ e@(S.EAnd _ e1 e2) = do
    type1 <- typeOf' e1
    type2 <- typeOf' e2
    expecttype2 TBool type1 type2 TBool (S.hasPosition e) ("cannot and: " ++ (typeShow type1) ++ " and " ++ (typeShow type2))
typeOf_ e@(S.EOr _ e1 e2) = do
    type1 <- typeOf' e1
    type2 <- typeOf' e2
    expecttype2 TBool type1 type2 TBool (S.hasPosition e) ("cannot or: " ++ (typeShow type1) ++ " and " ++ (typeShow type2))
typeOf_ e@(S.ETuple _ exprs) = do
    types <- mapM typeOf' exprs
    return $ TTuple types
typeOf_ e@(S.EApp _ fun args) = do
    fun_type <- typeOf' fun 
    real_arg_types <- mapM typeOf_ args
    case fun_type of
        TFun ret_type arg_types -> 
            if real_arg_types == arg_types 
            then return ret_type 
            else formatError (S.hasPosition e) ("cannot call: " ++ (show fun) ++ " with " ++ (concat (map typeShow real_arg_types)))
        _ -> formatError (S.hasPosition e) ("not callable:" ++ (show fun))


getdeclarations :: S.Block -> TypeEnv -> Except String (TypeEnv, S.Block)
getdeclarations bl@(S.BBlock _ declarations) env =
    let (decls, nondecls) = (filter isdecl declarations, join $ fmap decltoass declarations) in
        case runIdentity (runExceptT (foldM decltoenv env decls)) of
            Left err -> throwE err
            Right env_ -> return (env_, (S.BBlock (S.hasPosition bl) nondecls))
    where
        isdecl :: S.Stmt -> Bool
        isdecl (S.Decl _ _ _) = True
        isdecl _ = False
        decltoass :: S.Stmt -> [S.Stmt]
        decltoass (S.Decl _ type_ items) = [(S.Ass pos ident expr) | (S.Init pos ident expr) <- items]
        decltoass x = [x] 
        decltoenv :: TypeEnv -> S.Stmt -> Except String TypeEnv
        decltoenv e (S.Decl _ type_ items) = 
            case (foldM_ (\_ _ -> return ()) () [typeOf expr env | S.Init _ (S.Ident _) expr <- items]) of
                Left err -> throwE err
                _ -> return $ M.union e (M.fromList $ getvars (typeOfStype type_) items)
        decltoenv _ _ = undefined
        getvars :: Type -> [S.Item] -> [(String, Type)]
        getvars type_ items = [(ident, type_) | S.Init _ (S.Ident ident) _ <- items] ++ [(ident, type_) | S.NoInit _ (S.Ident ident) <- items]

typeCheckStmt :: S.Stmt -> TypeCheckerMonad ()
typeCheckStmt (S.BStmt _ bl@(S.BBlock _ _)) = do
    env <- ask
    case runExcept (getdeclarations bl env) of
        Right (env', (S.BBlock _ l')) -> local (const env') (f l')
        Left err -> throwE err
    where
        f [] = return ()
        f (a:b) = typeCheckStmt a >> f b

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
        Just TVoid -> return ()
        _ -> formatError (S.hasPosition st) $ "cannot return void from nonvoid function"
typeCheckStmt st@(S.Ret _ e) = do
    env <- ask
    case (M.lookup returnRegister env, typeOf e env) of
        (Nothing, _) -> formatError (S.hasPosition st) " fatal error: return register not allocated"
        (Just t, Right e_t) -> 
            cmptypes t e_t (S.hasPosition st) $ "Cannot return expression of type " ++ (show e_t) ++ " from function with type " ++ (show t)
        (_, Left err) -> formatError (S.hasPosition st) err



typeCheckStmt st@(S.Cond _ e s) = do
    env <- ask
    (case typeOf e env of
        Right TBool -> return ()
        Right _ -> formatError (S.hasPosition st) "condition is not bool"
        Left err -> formatError (S.hasPosition st) err) >> typeCheckStmt s

typeCheckStmt st@(S.CondElse _ e s1 s2) = do
    env <- ask
    (case typeOf e env of
        Right TBool -> return ()
        Right _ -> formatError (S.hasPosition st) "condition is not bool"
        Left err-> formatError (S.hasPosition st) err) >> typeCheckStmt s1 >> typeCheckStmt s2

typeCheckStmt st@(S.SExp _ e) = do
    env <- ask
    case typeOf e env of
        Right _ -> return ()
        Left err -> formatError (S.hasPosition st) err
        
typeCheckStmt st@(S.While _ e s) = do
    env <- ask
    (case typeOf e env of
        Right TBool -> return ()
        Right _ -> formatError (S.hasPosition st) "condition is not bool"
        Left err -> formatError (S.hasPosition st) err) >> typeCheckStmt s

    
typeCheckStmt xd = formatError Nothing $ "ajja: " ++ (show xd)
--typeCheckStmt _ = return ()
argtype :: S.Arg -> S.Type
argtype (S.VarArg _ t _) = t
argtype (S.RefArg _ t _) = t
argname :: S.Arg -> String
argname (S.VarArg _ _ (S.Ident i)) = i
argname (S.RefArg _ _ (S.Ident i)) = i

parseFunctionSig :: S.TopDef -> (String, Type)
parseFunctionSig (S.FnDef _ rettype (S.Ident ident) args _) = (ident, TFun (typeOfStype rettype) (map (typeOfStype . argtype) args))
functionArgNames :: S.TopDef -> [(String, Type)]
functionArgNames (S.FnDef _ _ _ args _) = zip (map argname args) (map (typeOfStype . argtype) args)

typeCheckProgram :: S.Program -> Except String ()
typeCheckProgram (S.PProgram _ fns) = do
    let fnsigs = map parseFunctionSig fns in
        let defs = M.fromList fnsigs in
            typeCheckProgram_ fns defs (map snd fnsigs)


typeCheckProgram_ :: [S.TopDef] -> TypeEnv -> [Type] -> Except String ()
typeCheckProgram_ [] _ _ = return ()
typeCheckProgram_ (td@(S.FnDef pos _ _ args block):b) env ((TFun rettype _):r) = do
        case runIdentity (runReaderT (runExceptT (typeCheckStmt (S.BStmt pos block))) env') of
            Left err -> throwE err
            Right _ -> typeCheckProgram_ b env r
    where env' = M.union (M.insert returnRegister rettype env) (M.fromList $ functionArgNames td)
    

{-
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
-}
