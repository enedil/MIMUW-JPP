{-# LANGUAGE RankNTypes #-}
module TypeChecker where

import Control.Monad.Reader --(ReaderT, ask, runReader, local)
import Data.Functor.Identity
import Control.Monad.Trans.Except
--import Control.Monad.Trans.Reader
--import Control.Monad.Except
--import Control.Monad.Fail
import qualified Data.Map as M
import Data.List(intercalate)
import qualified AbsCerber as S
import Tracing

type Pos = S.BNFC'Position

returnRegister :: String
returnRegister = "return"
inLoopRegister :: String
inLoopRegister = "while"
mainFunction :: String
mainFunction = "main"
type TypeEnv = M.Map String Type

type TypeCheckerMonad a = ExceptT String (Reader TypeEnv) a

data Type = TInt | TStr | TBool | TVoid | TFun Type [Type] | TTuple [Type] deriving (Eq, Ord)
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
typeOfStype x = bug $ show x

formatError :: forall a. Pos -> String -> TypeCheckerMonad a
formatError (Just (line, col)) arg = throwE $ show line ++ ":" ++ show col ++ ": " ++ arg
formatError Nothing arg = throwE $ "(unknown): " ++ arg

cmptypes :: Type -> Type -> Pos -> String -> TypeCheckerMonad ()
cmptypes t1 t2 pos msg = if t1 == t2 then return () else formatError pos msg

typeShow :: Type -> String
typeShow TInt = "int"
typeShow TStr = "str"
typeShow TBool = "bool"
typeShow TVoid = "void"
typeShow (TFun ret args) = "function<" ++ (intercalate "," $ map typeShow (ret: args)) ++ ">"
typeShow (TTuple elems) = "tuple<" ++ (intercalate "," $ map typeShow elems) ++ ">"

typeOf :: S.Expr -> TypeEnv -> Either String Type
typeOf e = runReader (runExceptT $ typeOf_ e)

expecttype :: Pos -> Type -> Type -> Type -> String -> TypeCheckerMonad Type
expecttype pos expected input rettype err = if expected == input then return rettype else formatError pos err
expecttype2 :: Pos -> Type -> Type -> Type -> Type -> String -> TypeCheckerMonad Type
expecttype2 pos expected input1 input2 rettype err = 
    if expected == input1 && expected == input2
    then return rettype
    else formatError pos err
checkBinaryOp :: Pos -> S.Expr -> S.Expr -> Type -> Type -> String -> TypeCheckerMonad Type
checkBinaryOp pos e1 e2 expected rettype msg = do
    t1 <- typeOf' e1
    t2 <- typeOf' e2
    expecttype2 pos expected t1 t2 rettype $ msg ++ show t1 ++ " and " ++ show t2

typeOf' :: S.Expr -> TypeCheckerMonad Type
typeOf' x = trace ("expr=" ++ show x) $ typeOf_ x

typeOf_ :: S.Expr -> TypeCheckerMonad Type
typeOf_ (S.EVar _ (S.Ident name)) = do
    type_ <- asks (M.lookup name)
    case type_ of
        Just t -> return t
        Nothing -> throwE $ "Variable or function not in scope: " ++ name

typeOf_ (S.ELitInt _ _) = return TInt
typeOf_ (S.ELitTrue _) = return TBool 
typeOf_ (S.ELitFalse _) = return TBool
typeOf_ (S.EString _ _) = return TStr
typeOf_ (S.Neg p e_in) = do
    type_in <- typeOf' e_in
    expecttype p TInt type_in TInt ("cannot negate: " ++ show type_in)
typeOf_ (S.Not p e_in) = do
    type_in <- typeOf' e_in
    expecttype p TBool type_in TBool ("cannot invert: " ++ show type_in)
typeOf_ (S.EMul p e1 _ e2) = checkBinaryOp p e1 e2 TInt TInt "cannot multiply/divide: "
typeOf_ (S.EAdd p e1 _ e2) = checkBinaryOp p e1 e2 TInt TInt "cannot add/subtract: "
typeOf_ (S.ERel p e1 _ e2) = checkBinaryOp p e1 e2 TInt TBool "cannot compare: "
typeOf_ (S.EAnd p e1 e2) = checkBinaryOp p e1 e2 TBool TBool "cannot and: "
typeOf_ (S.EOr p e1 e2) = checkBinaryOp p e1 e2 TBool TBool "cannot or: "
typeOf_ (S.ETuple _ exprs) = do
    types <- mapM typeOf' exprs
    return $ TTuple types
typeOf_ (S.EApp _ fun args) = do
    fun_type <- typeOf' fun 
    real_arg_types <- mapM typeOf_ args
    case fun_type of
        TFun ret_type arg_types -> 
            if real_arg_types == arg_types 
            then return ret_type 
            else throwE $ "cannot call: " ++ show fun ++ " with " ++ (intercalate ", " $ map show real_arg_types)
        _ -> throwE $ "not callable:" ++ show fun


getdeclarations :: S.Block -> TypeEnv -> Except String (TypeEnv, S.Block)
getdeclarations bl@(S.BBlock _ declarations) env =
    let (decls, nondecls) = (filter isdecl declarations, decltoass =<< declarations) in
        case runIdentity (runExceptT (foldM decltoenv env decls)) of
            Left err -> throwE err
            Right env_ -> return (env_, S.BBlock (S.hasPosition bl) nondecls)
    where
        isdecl :: S.Stmt -> Bool
        isdecl (S.Decl _ _ _) = True
        isdecl _ = False
        decltoass :: S.Stmt -> [S.Stmt]
        decltoass (S.Decl _ _ items) = [S.Ass pos ident expr | (S.Init pos ident expr) <- items]
        decltoass x = [x] 
        decltoenv :: TypeEnv -> S.Stmt -> Except String TypeEnv
        decltoenv e (S.Decl _ type_ items) = 
            case sequence [typeOf expr env | S.Init _ (S.Ident _) expr <- items] of
                Left err -> throwE err
                _ -> let u = M.fromList $ getvars (typeOfStype type_) items in
                        if trace ("envs: " ++ show u ++ " -- " ++ show items) (M.size u == length items) 
                        then return (M.union e u)
                        else throwE "redeclaration of local variable"
        decltoenv e st = bug $ "decltoenv called with unexpected parameters " ++ show e ++ ", " ++ show st
        getvars :: Type -> [S.Item] -> [(String, Type)]
        getvars type_ items = [(ident, type_) | S.Init _ (S.Ident ident) _ <- items] ++ [(ident, type_) | S.NoInit _ (S.Ident ident) <- items]

typeCheckStmt :: S.Stmt -> TypeCheckerMonad ()
typeCheckStmt (S.BStmt p bl@(S.BBlock _ _)) = do
    env <- ask
    case runExcept $ getdeclarations bl env of
        Right (env', S.BBlock _ l') -> local (const env') (f l')
        Left err -> formatError p err
    where
        f [] = return ()
        f (a:b) = typeCheckStmt a >> f b

typeCheckStmt st@(S.Ass _ (S.Ident ident) expr) = do
    env <- ask
    case (M.lookup ident env, typeOf expr env) of
        (Just id_t, Right expr_t) -> 
            cmptypes id_t expr_t (S.hasPosition st) $ "can't assign type " ++ show expr_t ++ " to variable of type " ++ show id_t
        (Nothing, Right _) -> formatError (S.hasPosition st) $ "undefined variable " ++ ident
        (_, Left err) -> formatError (S.hasPosition st) err

typeCheckStmt st@(S.VRet _ ) = do
    env <- ask
    case M.lookup returnRegister env of
        Just TVoid -> return ()
        _ -> formatError (S.hasPosition st) "cannot return void from nonvoid function"
typeCheckStmt st@(S.Ret _ e) = do
    env <- ask
    case (M.lookup returnRegister env, typeOf e env) of
        (Nothing, _) -> formatError (S.hasPosition st) " fatal error: return register not allocated"
        (Just t, Right e_t) -> 
            cmptypes t e_t (S.hasPosition st) $ "Cannot return expression of type " ++ show e_t ++ " from function with type " ++ show t
        (_, Left err) -> formatError (S.hasPosition st) err



typeCheckStmt st@(S.Cond _ e s) = do
    env <- ask
    case typeOf e env of
        Right TBool -> return ()
        Right _ -> formatError (S.hasPosition st) "condition is not bool"
        Left err -> formatError (S.hasPosition st) err
    typeCheckStmt s

typeCheckStmt st@(S.CondElse _ e s1 s2) = do
    env <- ask
    case typeOf e env of
        Right TBool -> return ()
        Right _ -> formatError (S.hasPosition st) "condition is not bool"
        Left err-> formatError (S.hasPosition st) err
    typeCheckStmt s1
    typeCheckStmt s2

typeCheckStmt st@(S.SExp _ e) = do
    env <- ask
    case typeOf e env of
        Right _ -> return ()
        Left err -> formatError (S.hasPosition st) err
        
typeCheckStmt st@(S.While _ e s) = do
    env <- ask
    case typeOf e env of
        Right TBool -> return ()
        Right _ -> formatError (S.hasPosition st) "condition is not bool"
        Left err -> formatError (S.hasPosition st) err
    local (M.insert inLoopRegister TBool) (typeCheckStmt s)

typeCheckStmt st@(S.Break _) = checkIsInLoop st
typeCheckStmt st@(S.Continue _) = checkIsInLoop st
typeCheckStmt st@(S.MAss _ e1 e2) = do
    env <- ask
    case (typeOf e1 env, typeOf e2 env) of
        (Right t1, Right t2) -> if t1 == t2 then return () else formatError (S.hasPosition st) "types from := assignment do not match"
        (Left err, _) -> formatError (S.hasPosition st) err
        (_, Left err) -> formatError (S.hasPosition st) err
typeCheckStmt arg = bug $ "typeCheckProgram called with arg=" ++ show arg

checkIsInLoop :: S.Stmt -> TypeCheckerMonad ()
checkIsInLoop st = do
    inLoop <- asks (M.lookup inLoopRegister)
    case inLoop of
        Just _ -> return ()
        _ -> formatError (S.hasPosition st) "break/continue not inside loop"
    
argtype :: S.Arg -> S.Type
argtype (S.VarArg _ t _) = t
argtype (S.RefArg _ t _) = t
argname :: S.Arg -> String
argname (S.VarArg _ _ (S.Ident i)) = i
argname (S.RefArg _ _ (S.Ident i)) = i

parseTopLevelSig :: S.TopDef -> (String, Type)
parseTopLevelSig (S.FnDef _ rettype (S.Ident ident) args _) = (ident, TFun (typeOfStype rettype) (map (typeOfStype . argtype) args))
parseTopLevelSig (S.Global _ type_ (S.Ident ident)) = (ident, typeOfStype type_)
functionArgNames :: S.TopDef -> [(String, Type)]
functionArgNames (S.FnDef _ _ _ args _) = zip (map argname args) (map (typeOfStype . argtype) args)
functionArgNames _ = bug "functionArgNames called with a non-function"

builtins :: [(String, Type)]
builtins = [("print", TFun TVoid [TStr]), ("tostring", TFun TStr [TInt])]

typeCheckProgram :: S.Program -> Except String ()
typeCheckProgram (S.PProgram _ fns) = do
    let sigs = map parseTopLevelSig fns in
        let defs = M.fromList $ builtins ++ sigs in
            if M.lookup mainFunction defs /= Just (TFun TVoid [])
            then throwE "wrong main signature (or lack thereof)"
            else if M.size defs /= length (builtins ++ sigs)
            then throwE "top level redefinition"
            else typeCheckProgram_ fns defs (map snd sigs)


typeCheckProgram_ :: [S.TopDef] -> TypeEnv -> [Type] -> Except String ()
typeCheckProgram_ [] _ _ = return ()
typeCheckProgram_ (td@(S.FnDef pos _ _ _ block):b) env ((TFun rettype _):r) = do
    case runIdentity (runReaderT (runExceptT (typeCheckStmt (S.BStmt pos block))) env') of
        Left err -> throwE err
        Right _ -> typeCheckProgram_ b env r
    where env' = M.union (M.insert returnRegister rettype env) (M.fromList $ functionArgNames td)
typeCheckProgram_ (S.Global _ _ _:b) env (_:t) = typeCheckProgram_ b env t

-- Gdyby tylko mieć typy zależne, to bym nie musiał tego pisać.
typeCheckProgram_ x y z = bug $ "typecheckprogram_ " ++ show (x, y, z)
