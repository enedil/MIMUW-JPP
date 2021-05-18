{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

-- | Pretty-printer for PrintCerber.
--   Generated by the BNF converter.

module PrintCerber where

import Prelude
  ( ($), (.)
  , Bool(..), (==), (<)
  , Int, Integer, Double, (+), (-), (*)
  , String, (++)
  , ShowS, showChar, showString
  , all, dropWhile, elem, foldr, id, map, null, replicate, shows, span
  )
import Data.Char ( Char, isSpace )
import qualified AbsCerber

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i = \case
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    [";"]        -> showChar ';'
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : ts@(p:_) | closingOrPunctuation p -> showString t . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i     = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t s =
    case (all isSpace t', null spc, null rest) of
      (True , _   , True ) -> []              -- remove trailing space
      (False, _   , True ) -> t'              -- remove trailing space
      (False, True, False) -> t' ++ ' ' : s   -- add space if none
      _                    -> t' ++ s
    where
      t'          = showString t []
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt     _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q = \case
  s | s == q -> showChar '\\' . showChar s
  '\\' -> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  s -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print AbsCerber.Ident where
  prt _ (AbsCerber.Ident i) = doc $ showString i

instance Print (AbsCerber.Program' a) where
  prt i = \case
    AbsCerber.PProgram _ topdefs -> prPrec i 0 (concatD [prt 0 topdefs])

instance Print (AbsCerber.TopDef' a) where
  prt i = \case
    AbsCerber.FnDef _ type_ id_ args block -> prPrec i 0 (concatD [prt 0 type_, prt 0 id_, doc (showString "("), prt 0 args, doc (showString ")"), prt 0 block])
    AbsCerber.Global _ type_ id_ -> prPrec i 0 (concatD [prt 0 type_, prt 0 id_, doc (showString ";")])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print [AbsCerber.TopDef' a] where
  prt = prtList

instance Print (AbsCerber.Arg' a) where
  prt i = \case
    AbsCerber.VarArg _ type_ id_ -> prPrec i 1 (concatD [prt 0 type_, prt 0 id_])
    AbsCerber.RefArg _ type_ id_ -> prPrec i 2 (concatD [prt 0 type_, doc (showString "ref"), prt 0 id_])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [AbsCerber.Arg' a] where
  prt = prtList

instance Print (AbsCerber.Block' a) where
  prt i = \case
    AbsCerber.BBlock _ stmts -> prPrec i 0 (concatD [doc (showString "{"), prt 0 stmts, doc (showString "}")])

instance Print [AbsCerber.Stmt' a] where
  prt = prtList

instance Print (AbsCerber.Stmt' a) where
  prt i = \case
    AbsCerber.Empty _ -> prPrec i 0 (concatD [doc (showString ";")])
    AbsCerber.BStmt _ block -> prPrec i 0 (concatD [prt 0 block])
    AbsCerber.Decl _ type_ items -> prPrec i 0 (concatD [prt 0 type_, prt 0 items, doc (showString ";")])
    AbsCerber.Ass _ id_ expr -> prPrec i 0 (concatD [prt 0 id_, doc (showString "="), prt 0 expr, doc (showString ";")])
    AbsCerber.MAss _ expr1 expr2 -> prPrec i 0 (concatD [prt 7 expr1, doc (showString ":="), prt 0 expr2, doc (showString ";")])
    AbsCerber.Ret _ expr -> prPrec i 0 (concatD [doc (showString "return"), prt 0 expr, doc (showString ";")])
    AbsCerber.VRet _ -> prPrec i 0 (concatD [doc (showString "return"), doc (showString ";")])
    AbsCerber.VYield _ expr -> prPrec i 0 (concatD [doc (showString "yield"), prt 0 expr, doc (showString ";")])
    AbsCerber.Cond _ expr stmt -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 stmt])
    AbsCerber.CondElse _ expr stmt1 stmt2 -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 stmt1, doc (showString "else"), prt 0 stmt2])
    AbsCerber.SExp _ expr -> prPrec i 0 (concatD [prt 0 expr, doc (showString ";")])
    AbsCerber.Break _ -> prPrec i 0 (concatD [doc (showString "break"), doc (showString ";")])
    AbsCerber.Continue _ -> prPrec i 0 (concatD [doc (showString "continue"), doc (showString ";")])
    AbsCerber.While _ expr stmt -> prPrec i 0 (concatD [doc (showString "while"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 stmt])
    AbsCerber.ForIn _ id_ expr stmt -> prPrec i 0 (concatD [doc (showString "for"), prt 0 id_, doc (showString "in"), prt 0 expr, prt 0 stmt])
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print (AbsCerber.Item' a) where
  prt i = \case
    AbsCerber.NoInit _ id_ -> prPrec i 0 (concatD [prt 0 id_])
    AbsCerber.Init _ id_ expr -> prPrec i 0 (concatD [prt 0 id_, doc (showString "="), prt 0 expr])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [AbsCerber.Item' a] where
  prt = prtList

instance Print (AbsCerber.Type' a) where
  prt i = \case
    AbsCerber.Int _ -> prPrec i 0 (concatD [doc (showString "int")])
    AbsCerber.Str _ -> prPrec i 0 (concatD [doc (showString "string")])
    AbsCerber.Bool _ -> prPrec i 0 (concatD [doc (showString "boolean")])
    AbsCerber.Void _ -> prPrec i 0 (concatD [doc (showString "void")])
    AbsCerber.Fun _ type_ types -> prPrec i 0 (concatD [prt 0 type_, doc (showString "("), prt 0 types, doc (showString ")")])
    AbsCerber.Generator _ type_ -> prPrec i 0 (concatD [doc (showString "generator"), doc (showString "<"), prt 0 type_, doc (showString ">")])
    AbsCerber.Tuple _ types -> prPrec i 0 (concatD [doc (showString "tuple"), doc (showString "<"), prt 0 types, doc (showString ">")])
    AbsCerber.Function _ types -> prPrec i 0 (concatD [doc (showString "function"), doc (showString "<"), prt 0 types, doc (showString ">")])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [AbsCerber.Type' a] where
  prt = prtList

instance Print (AbsCerber.Expr' a) where
  prt i = \case
    AbsCerber.EVar _ id_ -> prPrec i 6 (concatD [prt 0 id_])
    AbsCerber.ELitInt _ n -> prPrec i 6 (concatD [prt 0 n])
    AbsCerber.ELitTrue _ -> prPrec i 6 (concatD [doc (showString "true")])
    AbsCerber.ELitFalse _ -> prPrec i 6 (concatD [doc (showString "false")])
    AbsCerber.EApp _ expr exprs -> prPrec i 6 (concatD [prt 6 expr, doc (showString "("), prt 0 exprs, doc (showString ")")])
    AbsCerber.EString _ str -> prPrec i 6 (concatD [prt 0 str])
    AbsCerber.Neg _ expr -> prPrec i 5 (concatD [doc (showString "-"), prt 6 expr])
    AbsCerber.Not _ expr -> prPrec i 5 (concatD [doc (showString "!"), prt 6 expr])
    AbsCerber.EMul _ expr1 mulop expr2 -> prPrec i 4 (concatD [prt 4 expr1, prt 0 mulop, prt 5 expr2])
    AbsCerber.EAdd _ expr1 addop expr2 -> prPrec i 3 (concatD [prt 3 expr1, prt 0 addop, prt 4 expr2])
    AbsCerber.ERel _ expr1 relop expr2 -> prPrec i 2 (concatD [prt 2 expr1, prt 0 relop, prt 3 expr2])
    AbsCerber.EAnd _ expr1 expr2 -> prPrec i 1 (concatD [prt 2 expr1, doc (showString "&&"), prt 1 expr2])
    AbsCerber.EOr _ expr1 expr2 -> prPrec i 0 (concatD [prt 1 expr1, doc (showString "||"), prt 0 expr2])
    AbsCerber.ETuple _ exprs -> prPrec i 7 (concatD [doc (showString "["), prt 0 exprs, doc (showString "]")])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [AbsCerber.Expr' a] where
  prt = prtList

instance Print (AbsCerber.AddOp' a) where
  prt i = \case
    AbsCerber.Plus _ -> prPrec i 0 (concatD [doc (showString "+")])
    AbsCerber.Minus _ -> prPrec i 0 (concatD [doc (showString "-")])

instance Print (AbsCerber.MulOp' a) where
  prt i = \case
    AbsCerber.Times _ -> prPrec i 0 (concatD [doc (showString "*")])
    AbsCerber.Div _ -> prPrec i 0 (concatD [doc (showString "/")])
    AbsCerber.Mod _ -> prPrec i 0 (concatD [doc (showString "%")])

instance Print (AbsCerber.RelOp' a) where
  prt i = \case
    AbsCerber.LTH _ -> prPrec i 0 (concatD [doc (showString "<")])
    AbsCerber.LE _ -> prPrec i 0 (concatD [doc (showString "<=")])
    AbsCerber.GTH _ -> prPrec i 0 (concatD [doc (showString ">")])
    AbsCerber.GE _ -> prPrec i 0 (concatD [doc (showString ">=")])
    AbsCerber.EQU _ -> prPrec i 0 (concatD [doc (showString "==")])
    AbsCerber.NE _ -> prPrec i 0 (concatD [doc (showString "!=")])

