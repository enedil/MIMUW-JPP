{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Interpreter where

import Control.Monad.Except
import Control.Monad.State
import qualified Control.Monad.Fail(MonadFail, fail)
import Control.Monad.Reader(MonadReader, ReaderT, runReaderT, local, ask, asks)
import System.IO

import Data.Maybe
import qualified Data.Map as M

import Tracing

import qualified AbsCerber as S
import qualified TypeChecker as TC


type Loc = Int
type Env = M.Map String Loc
type Pos = TC.Pos

data IState = IState {
    store :: M.Map Loc Value,
    lastLoc :: Loc
}

alloc :: InterpreterM Loc
alloc = state (\st -> (lastLoc st + 1, st {lastLoc = lastLoc st + 1}))

insertToStore :: Value -> Loc -> InterpreterM ()
insertToStore v l = modify (\st -> st {store = M.insert l v (store st)})

extractLoc :: String -> InterpreterM Loc
extractLoc name = do
    l <- asks (trace ("extract - " ++ name) (M.lookup name))
    case l of
        Just x -> return x
        Nothing -> fail $ "cannot extract name (this is a bug):" ++ name

extractVal :: String -> InterpreterM Value
extractVal ident = do
    l <- extractLoc ident
    v <- gets $ M.lookup l . store
    case v of
        Just val -> return val
        Nothing -> fail $ "value not in store: " ++ show ident

newtype InterpreterM a = InterpreterM {
  runInterpreterM :: ExceptT String (ReaderT Env (StateT IState IO)) a
} deriving (Applicative, Monad, Functor, MonadError String, MonadState IState, MonadReader Env, MonadIO)

instance Control.Monad.Fail.MonadFail InterpreterM where
  fail = throwError

formatError :: forall a. Pos -> String -> InterpreterM a
formatError (Just (line, col)) arg = fail $ show line ++ ":" ++ show col ++ ": " ++ arg
formatError Nothing arg = fail $ "(unknown): " ++ arg

data Arg = ValArg Value | RefArg Value deriving (Show)
data Fun = FVal (Value -> InterpreterM Fun) | FRef (Loc -> InterpreterM Fun) | FBottom (InterpreterM Value) 
instance Show Fun where
    show (FVal _) = "fval"
    show (FRef _) = "fref"
    show (FBottom _) = "fbottom"
data Value = StringV String | IntV Int | BoolV Bool | VoidV | FunV Fun | TupleV [Value] deriving (Show, Eq)
instance Eq Fun where
    (==) _ _ = False

makeFnDef :: [S.Stmt] -> [S.Arg] -> InterpreterM Fun
makeFnDef body (S.VarArg _ _ (S.Ident ident): rest) = do
        env <- ask
        return $ FVal (\val -> local (const env) $ do
            l <- alloc
            insertToStore val l
            local (M.insert ident l) (makeFnDef body rest))

makeFnDef body (S.RefArg _ _ (S.Ident ident): rest) = do
        env <- ask
        return $ FRef (\loc -> local (const env) $ do
            local (M.insert ident loc) (makeFnDef body rest))

makeFnDef body [] = do
    env <- ask
    return $ FBottom (retval . fromJust <$> local (const env) (exec body))

vbool :: Value -> Bool
vbool (BoolV b) = b
vbool _ = undefined
vint :: Value -> Int
vint (IntV i) = i
vint _ = undefined
vfun :: Value -> Fun
vfun (FunV fn) = fn
vfun _ = undefined
vstring :: Value -> String
vstring (StringV s) = s
vstring _ = undefined
retval :: RetType -> Value
retval (TReturn v) = v
retval _ = undefined

data RetType = TReturn Value | TBreak | TContinue
justReturn :: Value -> Maybe RetType
justReturn = Just . TReturn

exec :: [S.Stmt] -> InterpreterM (Maybe RetType)
exec [] = return Nothing
exec (S.Empty _: rest) = exec rest
exec (S.Decl _ _ []: rest) = exec rest
exec (S.Decl pos type_ ((S.NoInit _ (S.Ident ident)):b): rest) = do
    loc <- alloc
    local (M.insert ident loc) (exec (S.Decl pos type_ b: rest))
exec (S.Decl pos type_ ((S.Init _ (S.Ident ident) e):b): rest) = do
    loc <- alloc
    v <- eval e
    insertToStore v loc
    local (M.insert ident loc) (exec (S.Decl pos type_ b: rest))
exec (S.Ass _ (S.Ident ident) e: rest) = do
    v <- eval e
    l <- extractLoc ident
    insertToStore v l
    exec rest
exec (S.VRet _: _) = return $ justReturn VoidV
exec (S.Ret _ e: _) = justReturn <$> eval e
exec (S.Break _: _) = return $ Just TBreak
exec (S.Continue _: _) = return $ Just TContinue
exec (S.Cond _ e s: rest) = do
    cond <- eval e
    if vbool cond
    then exec (s : rest)
    else exec rest
exec (S.CondElse _ e s1 s2: rest) = do
    cond <- eval e
    if vbool cond
    then exec (s1 : rest)
    else exec (s2 : rest)
exec (S.SExp _ e: rest) = eval e >> exec rest
exec r@(S.While _ e body: rest) = do
    cond <- eval e
    if vbool cond
    then do
        ret <- exec [body]
        case ret of
            Just (TReturn _) -> return ret
            Just TBreak -> exec rest
            _ -> exec r
    else exec rest
exec (S.BStmt _ (S.BBlock _ stmts): rest) = do
    x <- exec stmts
    case x of
        Just _ -> return x
        Nothing -> exec rest
exec (S.MAss pos e1 e2: rest) = do
        tupRight <- eval e2
        assignTup e1 tupRight
        exec rest
    where
        assignTup :: S.Expr -> Value -> InterpreterM ()
        assignTup (S.ETuple _ args) (TupleV right) = mapM_ (uncurry assignTup) (zip args right)
        assignTup (S.EVar _ (S.Ident ident)) right = do
            l <- extractLoc ident
            insertToStore right l
        assignTup e right = do
            v <- eval e
            if v == right 
            then return ()
            else formatError pos $ "values from left and right differ: " ++ show v ++ " and " ++ show right
            

eval :: S.Expr -> InterpreterM Value
eval (S.ELitInt _ i) = return $ IntV (fromInteger i)
eval (S.ELitTrue _) = return $ BoolV True
eval (S.ELitFalse _) = return $ BoolV False
eval (S.EString _ s) = return $ StringV s
eval (S.EVar _ (S.Ident ident)) = extractVal ident
eval (S.Neg _ e) = IntV <$> ((-) 0 . vint <$> eval e)
eval (S.Not _ e) = BoolV <$> (not . vbool <$> eval e)
eval (S.EMul pos e1 op e2) = do
        v1 <- eval e1
        v2 <- eval e2
        f op (vint v1) (vint v2)
    where
        f :: S.MulOp -> Int -> Int -> InterpreterM Value
        f (S.Times _) a b = return $ IntV $ a * b
        f (S.Div _) _ 0 = formatError pos "division by 0"
        f (S.Mod _) _ 0 = formatError pos "modular division by 0"
        f (S.Div _) a b = return $ IntV $ a `div` b
        f (S.Mod _) a b = return $ IntV $ a `mod` b
eval (S.EAdd _ e1 op e2) = do
        v1 <- eval e1
        v2 <- eval e2
        return $ IntV $ f op (vint v1) (vint v2)
    where
        f :: S.AddOp -> Int -> Int -> Int
        f (S.Plus _) = (+)
        f (S.Minus _) = (-)
eval (S.ERel _ e1 op e2) = do
        v1 <- eval e1
        v2 <- eval e2 
        return $ BoolV $ f op (vint v1) (vint v2)
    where
        f :: S.RelOp -> Int -> Int -> Bool
        f (S.LTH _) = (<)
        f (S.GTH _) = (>)
        f (S.LE _) = (<=)
        f (S.GE _) = (>=)
        f (S.EQU _) = (==)
        f (S.NE _) = (/=)
eval (S.EAnd _ e1 e2) = do
    v1 <- eval e1
    if vbool v1
    then eval e2
    else return $ BoolV False
eval (S.EOr _ e1 e2) = do
    v1 <- eval e1
    if vbool v1
    then return $ BoolV True
    else eval e2
eval (S.ETuple _ args) = TupleV <$> mapM eval args
eval (S.EApp pos e_fn args_) = do
        fn <- eval e_fn
        app (vfun fn) args_
    where
        app :: Fun -> [S.Expr] -> InterpreterM Value
        app (FVal f) (arg1: args) = do
            v1 <- eval arg1
            fn <- f v1
            app fn args
        app (FRef f) ((S.EVar _ (S.Ident ident)): args) = do
            l <- extractLoc ident
            fn <- f l
            app fn args
        app (FRef f) (arg1: args) = do
            v <- eval arg1
            l <- alloc
            insertToStore v l
            fn <- f l
            app fn args
        app (FBottom f) [] = f
        app _ _ = formatError pos "bug: fun application"


registerBuiltins :: InterpreterM () -> InterpreterM ()
registerBuiltins f = do
        insertToStore (FunV b_print)    (-10)
        insertToStore (FunV b_tostring) (-11)
        local (M.union (M.fromList [("print", -10), ("tostring", -11)])) f
    where
        b_print = FVal (\val -> return $ FBottom (liftIO (putStr $ vstring val) >> return VoidV))
        b_tostring = FVal (\val -> return $ FBottom (return $ StringV (show (vint val))))

makeInterpreter :: S.Program -> InterpreterM ()
makeInterpreter (S.PProgram _ topdefs) = do
        let (f_idents, funs) = unzip [(ident, (args, body)) | (S.FnDef _ _ (S.Ident ident) args (S.BBlock _ body)) <- topdefs] in do
            locs <- replicateM (length topdefs) alloc
            let env = (M.union (M.fromList $ zip (f_idents ++ [ident | S.Global _ _ (S.Ident ident) <- topdefs]) locs)) in do
                local env $ registerBuiltins $ f [(l, makeFnDef body args) | ((args, body), l) <- zip funs locs]
   where
    f :: [(Loc, InterpreterM Fun)] -> InterpreterM ()
    f [] = do
        FunV (FBottom main) <- extractVal "main"
        main >> return ()
    f ((l, fn):b) = do
        env <- ask
        f' <- (traceShow env fn)
        insertToStore (FunV f') l
        f b

runInterpreter :: InterpreterM a -> IO (Either String a)
runInterpreter i = evalStateT (runReaderT (runExceptT $ runInterpreterM i) M.empty) (IState {store=M.empty, lastLoc=0})

runProg :: S.Program -> IO ()
runProg prog = do
    ret <- runInterpreter (makeInterpreter prog) 
    case ret of
        Left err -> hPutStrLn stderr $ "runtime error: " ++ err
        Right _ -> return ()
