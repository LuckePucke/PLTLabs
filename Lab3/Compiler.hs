
module Compiler where

--import Annotated
import CPP.Abs -- (Program(..), Id(..), Type(..), Arg(..))
import CPP.Print
-- import CPP.ErrM
import TypeChecker (FunType(..))

import Control.Monad.RWS
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

-- type Env = (Sig, [Context])	-- functions and context stack
-- type Context = Map Id Type	-- variables with their types
-- data FunDef = FunDef { funArgs :: [Id], funStms :: [Stm] }
-- data FunType = FunType { funRet :: Type, funPars :: [Type] }
type Cxt = [Block]
type Block = Map Id Addr
type Sig = Map Id Fun			-- function type signature
data Fun = Fun { funId :: Id, funType :: FunType } 
instance Show Fun where
	show fun = case (funRet (funType fun)) of
		Type_void	-> (showId (funId fun)) ++ "(" ++ pars ++ ")V"
		Type_bool	-> (showId (funId fun)) ++ "(" ++ pars ++ ")Z"
		Type_int	-> (showId (funId fun)) ++ "(" ++ pars ++ ")I"
		where
		pars = showPars $ funPars (funType fun)

showId :: Id -> String
showId (Id str) = str

showPars :: [Type] -> String
showPars [] = ""
showPars (t:ts) = case t of
	Type_void	-> "V" ++ showPars ts
	Type_bool	-> "Z" ++ showPars ts
	Type_int	-> "I" ++ showPars ts

newtype Label = L { theLabel :: Int }
	deriving (Eq)
instance Show Label where
	show (L i) = "L" ++ show i

data St = St
	{ cxt			:: Cxt		-- Context.
	, limitLocals	:: Int		-- Maximal size for locals encountered.
	, currentStack	:: Int		-- Current stack size.
	, limitStack	:: Int		-- Maximal stack size encountered.
	, nextLabel		:: Label	-- Next jump label (persistent part of state)
	}

initSt :: St
initSt = St
	{ cxt = [Map.empty]
	, limitLocals	= 0
	, currentStack	= 0
	, limitStack	= 0
	, nextLabel		= L 0
	}

type Output = [String]

type Compile = RWS Sig Output St

type Addr = Int


builtin :: [(Id, Fun)]
builtin =
  [ (Id "printInt",	Fun (Id "Runtime/printInt")	$ FunType Type_void [Type_int])
  , (Id "readInt",	Fun (Id "Runtime/readInt")	$ FunType Type_void [])
  ]

compile :: String -> Program -> String
compile name prg@(PDefs defs) = unlines w
	where
	sigEntry def@(DFun _ f@(Id x) _ _ ) = (f, Fun (Id $ name ++ "/" ++ x) $ defToFunType def)
	sig = Map.fromList $ builtin ++ map sigEntry defs
	w	 = snd $ evalRWS (compile' name prg) sig initSt

compile' :: String -> Program -> Compile ()
compile' name prg@(PDefs defs) = do
	tell header
	mapM_ compileFun defs
	where
	header = [
		";; BEGIN HEADER"
		, ""
		, ".class public " ++ name
		, ".super java/lang/Object"
		, ""
		, ".method public <init>()V"
		, " .limit locals 1"
		, ""
		, " aload_0"
		, " invokespecial java/lang/Object/<init>()V"
		, " return"
		, ""
		, ".end method"
		, ""
		, ".method public static main([Ljava/lang/String;)V"
		, " .limit locals 1"
		, " .limit stack 1"
		, ""
		, " invokestatic " ++ name ++ "/main()I"
		, " pop"
		, " return"
		, ""
		, ".end method"
		, ""
		, ";; END HEADER"
		]

compileFun :: Def -> Compile ()
compileFun def@(DFun typ id args stms) = do
	-- function header
	tell [ "", ".method public static " ++ show (Fun id $ defToFunType def) ]

	-- prepare environment
	lab <- gets nextLabel
	put initSt{ nextLabel = lab }
	mapM_ (\ (ADecl _ i) -> newVar i) args

	-- compile statements
	w <- grabOutput $ do
		mapM_ compileStm stms

	-- output limits
	ll <- gets limitLocals
	ls <- gets limitStack
	tell	[ " .limit locals " ++ show ll
			, " .limit stack " ++ show ls ]

	-- output code, indented by 1
	tell $ map (\ s -> if null s then s else " " ++ s) w

	-- function footer
	tell [ "", ".end method"]


compileStm :: Stm -> Compile ()
compileStm s = do

	-- Printing a comment
	let top = stmTop s
	unless (null top) $ do
		tell ["", ";; " ++ top]
--		tell $ map (";; " ++) $ lines top
--		case s of SDecls{} -> return(); _ -> blank

	-- message for NYI
	let nyi = error $ "TODO: " ++ top

	case s of

		SInit t x e -> do
			compileExp e
			newVar x
			a <- lookupVar x
			emit $ Store a

		SExp e -> do
			compileExp e
			emit $ Pop

		SReturn e -> do
			compileExp e
			emit $ Return

		SBlock ss -> do
			enterBlock
			mapM_ compileStm ss
			exitBlock

		SWhile e s1 -> do
			start <- newLabel
			done <- newLabel
			emit $ Label start
			compileExp e
			emit $ IfZ done
			enterBlock
			compileStm s1
			exitBlock
			emit $ Goto start
			emit $ Label done

		_ -> nyi

compileExp :: Exp -> Compile ()
compileExp e = case e of
	
	EInt i -> do
		emit $ IConst i
	
	EId x -> do
		a <- lookupVar x
		emit $ Load a
	
	EApp f es -> do
		mapM_ compileExp es
		sig <- ask
		let fun = fromMaybe (error "unbound function") $ Map.lookup f sig
		emit $ Call fun
		emit $ Nop
	
	EPlus e1 e2 -> do
		compileExp e1
		compileExp e2
		emit $ Add
	
	ELt e1 e2 -> do
		compileExp e1
		compileExp e2
		yes	<- newLabel
		done <- newLabel
		emit $ IfLt yes
		emit $ IConst 0
		emit $ Goto done
		emit $ Label yes
		emit $ IConst 1
		emit $ Label done
	
	EAss x e1 -> do
		compileExp e1
		a <- lookupVar x
		emit $ Store a
		emit $ Load a
	
	_ -> error $ "TODO: " ++ show e

-- | Print a single instruction.	Also update stack limits
-- om vi ska updatera stacken, måste vi inte ta in den i nån form av input då? 
emit :: Code -> Compile ()
emit code = case code of
	Store addr -> do
		tell ["istore_" ++ show addr]
		decStack
	Load addr -> do
		tell ["iload_" ++ show addr]
		incStack
	
	IConst i -> do
		tell ["iconst_" ++ show i]
		incStack
	Pop -> do
		tell ["pop"]
		decStack
	Add -> do
		tell ["iadd"]
		decStack
	
	Return		-> tell ["return"]
	Call fun	-> tell [("invokestatic " ++ show fun)]
	Nop			-> tell ["nop"]
	
	Label l		-> tell [show l ++ ":"]
	Goto l		-> tell ["goto " ++ show l]
	
	IfZ l -> do
		tell ["if_ifeq " ++ show l]
		decStack
	IfLt l -> do
		tell ["if_icmplt " ++ show l]
		decStack
		decStack

incStack :: Compile ()
incStack = do
	cs <- gets currentStack
	ls <- gets limitStack
	let cs' = cs + 1
	if (cs' > ls) then
		modify $ \ st -> st { currentStack = cs', limitStack = cs' }
	else
		modify $ \ st -> st { currentStack = cs' }

decStack :: Compile ()
decStack = do
	cs <- gets currentStack
	let cs' = cs - 1
	modify $ \ st -> st { currentStack = cs' }

data Code
	= Store Addr	-- ^ Store stack content of type @Type@ in local variable @Addr@.
	| Load Addr		-- ^ Push stack content of type @Type@ from local variable @Addr@.

	| IConst Integer	-- ^ Put integer constant on the stack.
	| Pop				-- ^ Pop something of type @Type@ from the stack.
	| Return			-- ^ Return from method of type @Type@.
	| Add				-- ^ Add 2 top values of stack.

	| Call Fun			-- ^ Call function.
	| Nop

	| Label Label		-- ^ Define label.
	| Goto Label		-- ^ Jump to label.
	| IfZ Label			-- ^ If top of stack is 0, jump to label.
	| IfLt Label	-- ^ If prev <	top, jump.
	
	deriving (Show)

stmTop :: Stm -> String
stmTop stm = case stm of
	SWhile e _		-> "while (" ++ printTree e ++ ")"
	SIfElse e _ _	-> "if (" ++ printTree e ++ ")"
	SBlock _		-> ""
	_ 				-> printTree stm

newLabel :: Compile Label
newLabel = do
	L i <- gets nextLabel
	modify $ \ st -> st { nextLabel = L $ i + 1 }
	return $ L i

grabOutput :: Compile () -> Compile Output
grabOutput m = do
	r <- ask
	s <- get
	let ((), s', w) = runRWS m r s
	put s'
	return w

newVar :: Id -> Compile ()
newVar id = do
	(c:cs) <- gets cxt
	ll <- gets limitLocals
	emit $ Store ll
	modify $ \ st -> st { cxt = ((Map.insert id ll c):cs), limitLocals = succ ll }
	return ()

lookupVar :: Id -> Compile Addr
lookupVar id = do
	c <- gets cxt
	let addr = lookupVar' id c
	emit $ Load addr
	return addr

lookupVar' :: Id -> Cxt -> Addr
lookupVar' _ [] = error $ "pls no"
lookupVar' id (c:cs) = case (Map.lookup id c) of
	Just a	-> a
	Nothing	-> lookupVar' id cs

enterBlock :: Compile ()
enterBlock = do
	c <- gets cxt
	modify $ \ st -> st { cxt = (Map.empty:c) }

exitBlock :: Compile ()
exitBlock = do
	c <- gets cxt
	modify $ \ st -> st { cxt = tail c }

-- defToIdFun :: Def -> (Id, Fun)
-- defToIdFun (DFun typ id args stms) = Fun ((Id id), (FunType (typ, (map (\(ADecl atyp aid) -> atyp) args))))
-- 
-- newFuns :: Sig -> [(Id, Fun)] -> Sig
-- newFuns sig [] = sig
-- newFuns sig ((id, fun):idfuns) = newFuns (Map.insert id fun sig) idfuns

defToFunType :: Def -> FunType
defToFunType (DFun t _ args _) = FunType t $ map (\ (ADecl t' _) -> t') args
