{-# LANGUAGE LambdaCase #-}

module Annotated where

import Data.Char
import CPP.Abs (Id(..), Type(..), Arg(..))

data Program = PDefs [Def]
  deriving (Eq, Ord, Show, Read)

data Def = DFun Type Id [Arg] [Stm]
  deriving (Eq, Ord, Show, Read)

data Stm
    = SExp Type Exp
    | SDecls Type [Id]
    | SInit Type Id Exp
    | SReturn Type Exp
    | SWhile Exp Stm
    | SBlock [Stm]
    | SIfElse Exp Stm Stm
  deriving (Eq, Ord, Show, Read)

data Exp
    = ETrue
    | EFalse
    | EInt Integer
    | EDouble Double
    | EId Id
    | EApp Id [Exp]
    | EPostIncr Id
    | EPostDecr Id
    | EPreIncr Id
    | EPreDecr Id
    | ETimes Type Exp Exp
    | EDiv Type Exp Exp
    | EPlus Type Exp Exp
    | EMinus Type Exp Exp
    | ELt Type Exp Exp
    | EGt Type Exp Exp
    | ELtEq Type Exp Exp
    | EGtEq Type Exp Exp
    | EEq Type Exp Exp
    | ENEq Type Exp Exp
    | EAnd Exp Exp
    | EOr Exp Exp
    | EAss Id Exp
  deriving (Eq, Ord, Show, Read)

-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) ""
  where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : ts@(p:_) | closingOrPunctuation p -> showString t . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else ' ':s)

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` term
  closingOrPunctuation _   = False

  term :: String
  term = ")],;."

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print Id where
  prt _ (Id i) = doc (showString i)
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])

instance Print Program where
  prt i e = case e of
    PDefs defs -> prPrec i 0 (concatD [prt 0 defs])

instance Print Def where
  prt i e = case e of
    DFun type_ id args stms -> prPrec i 0 (concatD [prt 0 type_, prt 0 id, doc (showString "("), prt 0 args, doc (showString ")"), doc (showString "{"), prt 0 stms, doc (showString "}")])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])

instance Print Arg where
  prt i e = case e of
    ADecl type_ id -> prPrec i 0 (concatD [prt 0 type_, prt 0 id])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])

instance Print Stm where
  prt i = \case
    SExp type_ exp -> prPrec i 0 (concatD [parenth (prt 0 type_), prt 0 exp, doc (showString ";")])
    SDecls type_ ids -> prPrec i 0 (concatD [prt 0 type_, prt 0 ids, doc (showString ";")])
    SInit type_ id exp -> prPrec i 0 (concatD [prt 0 type_, prt 0 id, doc (showString "="), prt 0 exp, doc (showString ";")])
    SReturn type_ exp -> prPrec i 0 (concatD [doc (showString "return"), parenth (prt 0 type_), prt 0 exp, doc (showString ";")])
    SWhile exp stm -> prPrec i 0 (concatD [doc (showString "while"), doc (showString "("), prt 0 exp, doc (showString ")"), prt 0 stm])
    SBlock stms -> prPrec i 0 (concatD [doc (showString "{"), prt 0 stms, doc (showString "}")])
    SIfElse exp stm1 stm2 -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 exp, doc (showString ")"), prt 0 stm1, doc (showString "else"), prt 0 stm2])

  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])

instance Print Exp where
  prt i = \case
    ETrue                  -> prPrec i 15 (concatD [doc (showString "true")])
    EFalse                 -> prPrec i 15 (concatD [doc (showString "false")])
    EInt n                 -> prPrec i 15 (concatD [prt 0 n])
    EDouble d              -> prPrec i 15 (concatD [prt 0 d])
    EId id                 -> prPrec i 15 (concatD [prt 0 id])
    EApp id exps           -> prPrec i 15 (concatD [prt 0 id, doc (showString "("), prt 0 exps, doc (showString ")")])
    EPostIncr exp          -> prPrec i 14 (concatD [prt 15 exp, doc (showString "++")])
    EPostDecr exp          -> prPrec i 14 (concatD [prt 15 exp, doc (showString "--")])
    EPreIncr exp           -> prPrec i 13 (concatD [doc (showString "++"), prt 14 exp])
    EPreDecr exp           -> prPrec i 13 (concatD [doc (showString "--"), prt 14 exp])
    ETimes type_ exp1 exp2 -> prPrec i 12 (concatD [prt 12 exp1, doc (prtType type_ . showString ".*" ), prt 13 exp2])
    EDiv type_ exp1 exp2   -> prPrec i 12 (concatD [prt 12 exp1, doc (prtType type_ . showString "./" ), prt 13 exp2])
    EPlus type_ exp1 exp2  -> prPrec i 11 (concatD [prt 11 exp1, doc (prtType type_ . showString ".+" ), prt 12 exp2])
    EMinus type_ exp1 exp2 -> prPrec i 11 (concatD [prt 11 exp1, doc (prtType type_ . showString ".-" ), prt 12 exp2])
    ELt type_ exp1 exp2    -> prPrec i  9 (concatD [prt 10 exp1, doc (prtType type_ . showString ".<" ), prt 10 exp2])
    EGt type_ exp1 exp2    -> prPrec i  9 (concatD [prt 10 exp1, doc (prtType type_ . showString ".>" ), prt 10 exp2])
    ELtEq type_ exp1 exp2  -> prPrec i  9 (concatD [prt 10 exp1, doc (prtType type_ . showString ".<="), prt 10 exp2])
    EGtEq type_ exp1 exp2  -> prPrec i  9 (concatD [prt 10 exp1, doc (prtType type_ . showString ".>="), prt 10 exp2])
    EEq type_ exp1 exp2    -> prPrec i  8 (concatD [prt  9 exp1, doc (prtType type_ . showString ".=="), prt  9 exp2])
    ENEq type_ exp1 exp2   -> prPrec i  8 (concatD [prt  9 exp1, doc (prtType type_ . showString ".!="), prt  9 exp2])
    EAnd exp1 exp2         -> prPrec i  4 (concatD [prt 4 exp1, doc (showString "&&"), prt 5 exp2])
    EOr exp1 exp2          -> prPrec i  3 (concatD [prt 3 exp1, doc (showString "||"), prt 4 exp2])
    EAss exp1 exp2         -> prPrec i  2 (concatD [prt 3 exp1, doc (showString "=" ), prt 2 exp2])
   where
    prtType :: Type -> ShowS
    prtType t = concatS (prt 0 t [])

  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])

instance Print Type where
  prt i e = case e of
    Type_bool   -> prPrec i 0 (doc (showString "bool"  ))
    Type_int    -> prPrec i 0 (doc (showString "int"   ))
    Type_double -> prPrec i 0 (doc (showString "double"))
    Type_void   -> prPrec i 0 (doc (showString "void"  ))
