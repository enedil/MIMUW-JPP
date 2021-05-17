{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Interpreter where

import Control.Monad.Except
--import Control.Monad.Fail
import Control.Monad.State
import Data.Maybe

import Control.Monad.Reader --(ReaderT, ask, runReader, local)
--import Control.Monad.Identity
--import Control.Monad.Trans.Identity
--import Control.Monad.Trans.Reader
import qualified Data.Map as M
--import Debug.Trace

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
-- jdfiofjdsio

insertToStore :: Value -> Loc -> InterpreterM ()
insertToStore v l = modify (\st -> st {store = M.insert l v (store st)})

extractLoc :: String -> InterpreterM Loc
extractLoc name = do
    l <- asks $ M.lookup name
    case l of
        Just x -> return x
        Nothing -> fail "to nie tak miało być"

extractVal :: String -> InterpreterM Value
extractVal ident = do
    l <- extractLoc ident
    v <- gets $ (M.lookup l) . store
    case v of
        Just val -> return val
        Nothing -> fail "value not in store"

newtype InterpreterM a = InterpreterM {
  runInterpreterM :: ExceptT String (ReaderT Env (StateT IState IO)) a
} deriving (Applicative, Monad, Functor, MonadError String, MonadState IState, MonadReader Env, MonadIO)

instance MonadFail InterpreterM where
  fail = throwError

data Arg = ValArg Value | RefArg Value deriving (Show)
data Fun = FVal (Value -> InterpreterM Fun) | FRef (Loc -> InterpreterM Fun) | FBottom (InterpreterM Value) 
instance Show Fun where
    show (FVal _) = "fval"
    show (FRef _) = "fref"
    show (FBottom _) = "fbottom"
data Value = StringV String | IntV Int | BoolV Bool | VoidV | FunV Fun | TupleV [Value] deriving Show

makeFnDef :: [S.Stmt] -> [S.Arg] -> InterpreterM Fun
makeFnDef body (S.VarArg _ _ (S.Ident ident): rest) = do
        env <- ask
        return $ FVal (\val -> (local (const env) $ do
            l <- alloc
            insertToStore val l
            local (M.insert ident l) (makeFnDef body rest)))

makeFnDef body (S.RefArg _ _ (S.Ident ident): rest) = do
        env <- ask
        return $ FRef (\loc -> (local (const env) $ do
            local (M.insert ident loc) (makeFnDef body rest)))

makeFnDef body [] = do
    env <- ask
    return $ FBottom ((retval . fromJust) <$> local (const env) (exec body))

b_print = FVal (\val -> return $ FBottom $ ((liftIO $ putStr $ show (vstring val)) >> return VoidV))
b_tostring = FVal (\val -> return $ FBottom $ (return $ StringV (show (vint val))))

registerBuiltins = do
    insertToStore (FunV b_print)    438297434278947832843920753829487538472
    insertToStore (FunV b_tostring) 438297434278947833843920753829487538472

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
exec (S.Decl _ _ _:_) = fail "interpreter bug: Decl"
exec (S.Ass _ (S.Ident ident) e: rest) = do
    v <- eval e
    l <- extractLoc ident
    insertToStore v l
    exec rest
exec (S.VRet _: _) = return $ justReturn VoidV
exec (S.Ret _ e: _) = justReturn <$> eval e
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
exec (S.SExp _ e: rest) = do
    _ <- eval e
    exec rest
exec r@(S.While _ e body: rest) = do
    cond <- eval e
    if vbool cond
    then exec (body:r)
    else exec rest
exec (S.BStmt _ (S.BBlock _ stmts): rest) = do
    x <- exec stmts
    case x of
        Just v -> fmap Just (return v)
        Nothing -> exec rest
--exec (S.MAss )

eval :: S.Expr -> InterpreterM Value
eval (S.ELitInt _ i) = return $ IntV (fromInteger i)
eval (S.ELitTrue _) = return $ BoolV True
eval (S.ELitFalse _) = return $ BoolV False
eval (S.EString _ s) = return $ StringV s
eval (S.EVar _ (S.Ident ident)) = extractVal ident
eval (S.Neg _ e) = IntV <$> (((-) 0) . vint <$> eval e)
eval (S.Not _ e) = BoolV <$> (not . vbool <$> eval e)
eval (S.EMul _ e1 op e2) = do
        v1 <- eval e1
        v2 <- eval e2
        f op (vint v1) (vint v2)
    where
        f :: S.MulOp -> Int -> Int -> InterpreterM Value
        f (S.Times _) a b = return $ IntV $ a * b
        f (S.Div _) _ 0 = fail "division by 0"
        f (S.Mod _) _ 0 = fail "modular division by 0"
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
eval (S.EApp _ e_fn args) = do
    fn <- eval e_fn
    app (vfun fn) args
eval (S.ETuple _ args) = TupleV <$> (mapM eval args)

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
app _ _ = fail "bug: fun application"

runProg :: S.Program -> InterpreterM ()
runProg (S.PProgram _ topdefs) = do
    registerBuiltins
