
module Compiler where

import Annotated
import CPP.Abs
import CPP.Print
import CPP.ErrM
import TypeChecker

-- type Env = (Sig, [Context])	-- functions and context stack
-- type Context = Map Id Type	-- variables with their types
-- data FunDef = FunDef { funArgs :: [Id], funStms :: [Stm] }
-- data FunType = FunType { funRet :: Type, funPars :: [Type] }
type Sig = Map Id Fun			-- function type signature
data Fun = Fun { funId :: Id, funType FunType } 

builtin = [
		((Id "printInt"), (Fun (Id "Runtime/printInt") (VVoid [VInt]))),
		((Id "readInt"), (Fun (Id "Runtime/readInt") (VInt [])))
	]

compile
	:: String	-- Class name.
	-> Program	-- Type-annotated program.
	-> String	-- Generated jasmin source file content.
compile str prg = case (compile' str prg) of
	Just a	-> unlines a
	Nothing	-> ""

compile' :: String -> Program -> Maybe [String]
compile' str prg@(PDefs defs) = do
	let sig = newFuns Map.empty (builtin ++ (map defToIdFun defs))
	tell header
	map compileFun defs
	where
	header = [
	";; BEGIN HEADER"
	, ""
	, ".class public " ++ name
	, ".super java/lang/Object"
	, ""
	, ".method public <init>()V"
	, "  .limit locals 1"
	, ""
	, "  aload_0"
	, "  invokespecial java/lang/Object/<init>()V"
	, "  return"
	, ""
	, ".end method"
	, ""
	, ".method public static main([Ljava/lang/String;)V"
	, "  .limit locals 1"
	, "  .limit stack  1"
	, ""
	, "  invokestatic " ++ name ++ "/main()I"
	, "  pop"
	, "  return"
	, ""
	, ".end method"
	, ""
	, ";; END HEADER"
	]

compileFun :: lol -> String

defToIdFun :: Def -> (Id, Fun)
defToIdFun (DFun typ id args stms) = Fun ((Id id), (FunType (typ, (map (\(ADecl atyp aid) -> atyp) args))))

newFuns :: Sig -> [(Id, Fun)] -> Sig
newFuns sig [] = sig
newFuns sig ((id, fun):idfuns) = newFuns (Map.insert id fun sig) idfuns

