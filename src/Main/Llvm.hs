{-# OPTIONS -fwarn-unused-imports -fwarn-incomplete-patterns -fno-warn-type-defaults #-}

-- | Wrappers for compiler stages dealing with LLVM code.
module Main.Llvm
	(compileViaLlvm)
where

-- main stages
import Main.Setup
import Main.Sea
import Main.Util

import DDC.Base.DataFormat
import DDC.Base.Literal
import DDC.Base.SourcePos
import DDC.Main.Error
import DDC.Main.Pretty
import DDC.Var

import qualified Module.Scrape		as M
import qualified DDC.Main.Arg		as Arg
import qualified DDC.Config.Version	as Version

import Llvm
import LlvmM
import Llvm.Invoke
import Llvm.GhcReplace.Unique
import Llvm.Runtime
import Llvm.Runtime.Apply
import Llvm.Runtime.Data
import Llvm.Runtime.Error
import Llvm.Runtime.Tags
import Llvm.Util

import Sea.Exp
import Sea.Util				(eraseAnnotsTree)
import Sea.Pretty

import Util
import qualified Data.Map		as Map

import qualified Debug.Trace		as Debug

stage = "Main.Llvm"

debug = True

_trace s v
 =	if debug
	  then Debug.trace s v
	  else v


compileViaLlvm
	:: (?verbose :: Bool, ?pathSourceBase :: FilePath)
	=> Setup			-- ^ Compile setup.
	-> ModuleId			-- ^ Module to compile, must also be in the scrape graph.
	-> Tree ()			-- ^ The Tree for the module.
	-> FilePath			-- ^ FilePath of source file.
	-> [FilePath]			-- ^ C import directories.
	-> [FilePath]			-- ^ C include files.
	-> Map ModuleId [a]		-- ^ Module import map.
	-> Bool				-- ^ Module defines 'main' function.
	-> M.Scrape			-- ^ ScrapeGraph of this Module.
	-> Map ModuleId M.Scrape	-- ^ Scrape graph of all modules reachable from the root.
	-> Bool				-- ^ Whether to treat a 'main' function defined by this module
					--	as the program entry point.
	-> IO Bool

compileViaLlvm
	setup modName eTree pathSource importDirs includeFilesHere importsExp
	modDefinesMainFn sRoot scrapes_noRoot blessMain
 = do
	let ?args		= setupArgs setup

	outVerb $ ppr $ "  * Write C header\n"
	writeFile (?pathSourceBase ++ ".ddc.h")
		$ makeSeaHeader
			eTree
			pathSource
			(map ((\(Just f) -> f) . M.scrapePathHeader)
					$ Map.elems scrapes_noRoot)
			includeFilesHere

	outVerb $ ppr $ "  * Generating LLVM IR code\n"
	llvmSource	<- outLlvm modName eTree pathSource

	writeFile (?pathSourceBase ++ ".ddc.ll")
			$ ppLlvmModule llvmSource

	invokeLlvmCompiler ?pathSourceBase []
	invokeLlvmAssembler ?pathSourceBase []

	return modDefinesMainFn


-- | Create LLVM source files
outLlvm
	:: (?args :: [Arg.Arg])
	=> ModuleId
	-> (Tree ())		-- sea source
	-> FilePath		-- path of the source file
	-> IO LlvmModule

outLlvm moduleName eTree pathThis
 = do
	-- Break up the sea into parts.
	let 	([ 	_seaProtos, 		seaSupers
		 , 	_seaCafProtos,		seaCafSlots,		seaCafInits
		 ,	_seaData
		 , 	_seaHashDefs ], junk)

		 = partitionFs
			[ (=@=) PProto{}, 	(=@=) PSuper{}
			, (=@=) PCafProto{},	(=@=) PCafSlot{},	(=@=) PCafInit{}
			, (=@=) PData{}
			, (=@=) PHashDef{} ]
			eTree

	when (not $ null junk)
	 $ panic "Main.Llvm" $ "junk sea bits = " ++ show junk ++ "\n"

	-- Build the LLVM code
	let comments =	[ "---------------------------------------------------------------"
			, "      source: " ++ pathThis
			, "generated by: " ++ Version.ddcName
			, "" ]

	let aliases = [ ("struct.Obj", ddcObj) ]

	let code = seaCafInits ++ seaSupers

	let fwddecls	= [panicOutOfSlots, allocCollect, force, apply1FD, apply2FD, apply3FD, apply4FD, baseFalse, baseTrue]

	let globals	= moduleGlobals
			++ (catMap llvmOfSeaGlobal $ eraseAnnotsTree seaCafSlots)

	decls		<- mapM llvmOfSeaDecls $ eraseAnnotsTree code

	return $ LlvmModule comments aliases globals fwddecls decls




llvmOfSeaDecls :: Top (Maybe a) -> IO LlvmFunction
llvmOfSeaDecls (PSuper v p t ss)
 = do	blocks	<- evalStateT (llvmOfFunc ss) initLlvmState
	return	$
		LlvmFunction
		(LlvmFunctionDecl (seaVar False v) External CC_Ccc (toLlvmType t) FixedArgs (map llvmOfParams p) Nothing)
		(map (seaVar True . fst) p)	-- funcArgs
		[]				-- funcAttrs
		Nothing				-- funcSect
		[ LlvmBlock (fakeUnique "entry") blocks ]

llvmOfSeaDecls (PCafInit v t ss)
 = panic stage "Implement 'llvmOfSeaDecls (PCafInit v t ss)'"

llvmOfSeaDecls x
 = panic stage $ "Implement 'llvmOfSeaDecls (" ++ show x ++ ")'"



llvmOfParams :: (Var, Type) -> LlvmParameter
llvmOfParams (v, t) = (toLlvmType t, [])


llvmOfSeaGlobal :: Top (Maybe a) -> [LMGlobal]
llvmOfSeaGlobal (PCafSlot v t)
 | t == TPtr (TPtr TObj)
 =	let	tt = toLlvmType t
		var = LMGlobalVar
 			("_ddcCAF_" ++ seaVar False v)	-- Variable name
			tt				-- LlvmType
			ExternallyVisible		-- LlvmLinkageType
			Nothing				-- LMSection
			ptrAlign			-- LMAlign
			False				-- LMConst
	in [(var, Just (LMStaticLit (LMNullLit tt)))]

 | otherwise
 = panic stage $ "llvmOfSeaGlobal on : \n\tVar  : " ++ seaVar False v ++ "\n\tType : " ++ show t

llvmOfSeaGlobal x
 = panic stage $ "llvmOfSeaGlobal on : " ++ show x

moduleGlobals :: [LMGlobal]
moduleGlobals
 = 	[ ( ddcSlotPtr	, Nothing )
	, ( ddcSlotMax	, Nothing )
	, ( ddcSlotBase	, Nothing )
	, ( ddcHeapPtr	, Nothing )
	, ( ddcHeapMax	, Nothing ) ]


llvmOfFunc :: [Stmt a] -> LlvmM [LlvmStatement]
llvmOfFunc []
 =	endFunction

llvmOfFunc ss
 = do	mapM_	llvmOfStmt ss
	endFunction

llvmOfStmt :: Stmt a -> LlvmM ()
llvmOfStmt stmt
 = case stmt of
	SBlank		-> addComment "Blank"
	SEnter n	-> runtimeEnter n
	SLeave n	-> runtimeLeave n
	SComment s	-> addComment s
	SGoto loc	-> addBlock [Branch (LMNLocalVar (seaVar False loc) LMLabel)]
	SAssign v1 t v2 -> llvmOfAssign v1 t v2
	SReturn v	-> llvmOfReturn v
	SSwitch e a	-> llvmSwitch e a
	SLabel l	-> branchLabel (seaVar False l)

	-- LLVM is SSA so auto variables do not need to be declared.
	SAuto v t	-> addComment $ "SAuto " ++ seaVar True v ++ " " ++ show t

	_
	  -> panic stage $ "llvmOfStmt " ++ (take 150 $ show stmt)

--------------------------------------------------------------------------------

llvmSwitch :: Exp a -> [Alt a] -> LlvmM ()
llvmSwitch (XTag (XSlot v (TPtr TObj) i)) alt
 = do	addComment	$ "---------------------------------------\n"
			++ "llvmSwitch : " ++ seaVar False v
	reg		<- readSlot i
	tag		<- getObjTag reg
	addComment	$ "llvmSwitch : " ++ show tag
			++ "\n-----------------------------------------"


	switchEnd	<- lift $ newUnique "switch.end"
	switchDef	<- lift $ newUnique "switch.default"

	let switchEndL	= (LMLocalVar switchEnd LMLabel)
	let switchDefL	= (LMLocalVar switchDef LMLabel)

	lift $ mapM_ (putStrLn . show) alt

	let (def, rest)
			= partition (\ s -> s =@= ADefault{} || s =@= ACaseDeath{}) alt

	alts		<- lift $ mapM (genAltVars switchEndL) rest

	addBlock	[ Switch tag switchDefL (map fst alts) ]

	mapM_		genAltBlock alts

	if null def
	  then	addBlock [ Branch switchEndL ]
	  else	mapM_ (genAltDefault switchDef) def

	addBlock	[ MkLabel switchEnd ]

llvmSwitch e _
 = 	panic stage $ "llvmSwitch : " ++ show e


genAltVars :: LlvmVar -> Alt a -> IO ((LlvmVar, LlvmVar), Alt a)
genAltVars switchEnd alt@(ASwitch (XCon v) [])
 | varName v == "True"
 =	return ((i32LitVar 1, switchEnd), alt)

 | varName v == "Unit"
 =	return ((i32LitVar 0, switchEnd), alt)

genAltVars _ alt@(ACaseSusp (XSlot v t i) label)
 = do	lab	<- newUniqueLabel "susp"
	return	((tagSusp, lab), alt)

genAltVars _ alt@(ACaseIndir (XSlot v t i) label)
 = do	lab	<- newUniqueLabel "indir"
	return	((tagIndir, lab), alt)

genAltVars _ (ADefault _)
 = panic stage "getAltVars : found ADefault."

genAltVars _ x
 = panic stage $ "getAltVars : found " ++ show x


genAltBlock :: ((LlvmVar, LlvmVar), Alt a) -> LlvmM ()
genAltBlock ((_, lab), ACaseSusp (XSlot v t i) label)
 = do	addBlock	[ MkLabel (uniqueOfLlvmVar lab) ]
	obj		<- readSlot i
	forced		<- forceObj obj
	writeSlot	forced i
	addBlock	[ Branch lab ]

genAltBlock ((_, lab), ACaseIndir (XSlot v t i) label)
 = do	addBlock [ MkLabel (uniqueOfLlvmVar lab) ]
	obj		<- readSlot i
	followed	<- followObj obj
	writeSlot	followed i
	addBlock	[ Branch lab ]

genAltBlock ((_, lab), ASwitch (XCon _) [])
 =	addBlock [ Branch lab ]

genAltBlock ((_, lab), x)
 = do	panic stage $ "getAltBlock : " ++ show x

genAltDefault :: Unique -> Alt a -> LlvmM ()
genAltDefault label (ADefault ss)
 = do	addBlock [ MkLabel label ]
	mapM_ llvmOfStmt ss

genAltDefault label (ACaseDeath s@(SourcePos (n,l,c)))
 = do	addBlock
		[ MkLabel label
		, Expr (Call StdCall (funcOfDecl deathCase) [i32LitVar 0, i32LitVar l, i32LitVar c] [])
		, Unreachable
		]

	panic stage $ "getAltDefault : " ++ show s

genAltDefault _ def
 =	panic stage $ "getAltDefault : " ++ show def

--------------------------------------------------------------------------------

llvmOfAssign :: Exp a -> Type -> Exp a -> LlvmM ()
llvmOfAssign (XVar v1 t1) t@(TPtr (TPtr TObj)) (XVar v2 t2)
 | t1 == t && t2 == t && isGlobalVar v1 && isGlobalVar v2
 = do	src	<- lift $ newUniqueReg (toLlvmType t1)
	addBlock $
		[ Assignment src (loadAddress (toLlvmVar v2 t2))
		, Store src (pVarLift (toLlvmVar v1 t1)) ]

llvmOfAssign (XVar v1 t1) t@(TPtr (TPtr TObj)) (XVar v2 t2@(TPtr (TPtr TObj)))
 | t1 == t
 = do	reg		<- lift $ newUniqueReg (toLlvmType t1)
	addBlock	[ Assignment reg (Load (toLlvmVar v2 t2))
			, Store reg (toLlvmVar v1 t1) ]

llvmOfAssign (XVar v1 t1@(TPtr (TPtr TObj))) t@(TPtr TObj) x@(XPrim op args)
 = do	dstreg		<- llvmOfPtrManip (toLlvmType t1) op args
	addBlock	[ Store dstreg (pVarLift (toLlvmVar v1 t1)) ]

llvmOfAssign (XVar v1 t1) t@(TPtr TObj) (XSlot v2 t2 i)
 | t1 == t && t2 == t
 = do	let dst		= toLlvmVar v1 t1
	readSlotVar i dst

llvmOfAssign (XSlot v1 t1 i) t@(TPtr TObj) (XVar v2 t2)
 | t1 == t && t2 == t
 =	writeSlot (toLlvmVar v2 t2) i

llvmOfAssign ((XSlot v1 t1 i)) t@(TPtr TObj) (XBox t2 exp)
 | t1 == t
 = do	boxed		<- boxExp t2 exp
	writeSlot	boxed i

llvmOfAssign ((XSlot v1 t1 i1)) t@(TPtr TObj) x@(XApply (XSlot v2 t2 i2) args)
 | t1 == t && t2 == t
 = do	fptr		<- readSlot i2
	result		<- llvmFunApply fptr t2 args
	writeSlot	result i1


llvmOfAssign ((XSlot v1 t1 i1)) t@(TPtr TObj) x@(XCall v2 args)
 | t1 == t
 = do	result		<- lift $ newUniqueNamedReg "result" pObj
	addBlock	[ Assignment result (Call TailCall (toLlvmGlobalFunc v2 t args) [] []) ]
	writeSlot	result i1


{-
llvmOfAssign ((XSlot v1 t1 i1)) t@(TPtr TObj) (XAllocThunk var airity args)
 = do	addComment	$ "slot [" ++ show i1 ++ "] = allocThunk ("
			++ seaVar False var ++ "," ++ show airity
			++ "," ++ show args ++ ")"
	result		<- allocThunk (seaVar False var) airity args
	writeSlot	result i1
-}






llvmOfAssign (XVar v1 t1@(TCon vt1 _)) (TCon vt _) x@(XPrim op args)
 | varName vt1 == "Int32#" && varName vt == "Int32#"
 = do	dstreg		<- llvmOfXPrim i32 op args
	addBlock	[ Comment ["Dummy add 0."]
			, Assignment (toLlvmVar v1 t1) (LlvmOp LM_MO_Add dstreg (i32LitVar 0)) ]

llvmOfAssign (XVar v1 t1@(TCon vt1 _)) (TCon vt _) exp
 | varName vt1 == "Int32#" && varName vt == "Int32#"
 = do	dstreg		<- llvmVarOfExp exp
	addBlock	[ Comment ["Dummy add 0."]
			, Assignment (toLlvmVar v1 t1) (LlvmOp LM_MO_Add dstreg (i32LitVar 0)) ]


llvmOfAssign (XVarCAF v1 t1) t@TPtr{} (XInt 0)
 = do	addComment	$ "_ddcCAF_" ++ seaVar False v1 ++ " = NULL"
	dst		<- lift $ newUniqueReg ppObj
	addBlock	[ Assignment dst (loadAddress (pVarLift (toLlvmCafVar v1 t1)))
			, Store (LMLitVar (LMNullLit (toLlvmType t))) dst ]

llvmOfAssign (XVarCAF v1 t1) t@TPtr{} x@(XCall v2 args)
 = do	addComment	$ "_ddcCAF_" ++ seaVar False v1 ++ " = " ++ seaVar False v2 ++ " ()"
	dst1		<- lift $ newUniqueReg pObj
	dst2		<- lift $ newUniqueReg pObj
	addBlock	[ Assignment dst1 (loadAddress (pVarLift (toLlvmCafVar v1 t1)))
			, Assignment dst2 (Call TailCall (toLlvmGlobalFunc v2 t args) [] [])
			, Store dst2 (pVarLift dst1)
			]

llvmOfAssign a b c
 = panic stage $ "Unhandled : llvmOfAssign \n"
	++ take 150 (show a) ++ "\n"
	++ take 150 (show b) ++ "\n"
	++ take 150 (show c) ++ "\n"


llvmFunApply :: LlvmVar -> Type -> [Exp a] -> LlvmM LlvmVar
llvmFunApply fptr typ args
 = do	params	<- mapM llvmFunParam args
	addComment $ "llvmFunApply : " -- ++ show params
	applyN fptr params



llvmFunParam :: Exp a -> LlvmM LlvmVar
llvmFunParam (XVar v t@(TPtr TObj))
 =	return $ toLlvmVar v t

llvmFunParam (XSlot v (TPtr TObj) i)
 = 	readSlot i

llvmFunParam p
 = panic stage $ "llvmFunParam " ++ show p




boxExp :: Type -> Exp a -> LlvmM LlvmVar
boxExp t (XLit lit@(LiteralFmt (LInt value) (UnboxedBits 32)))
 = do	addComment $ "boxing1 " ++ show t
	boxInt32 $ i32LitVar value

boxExp t (XPrim op args)
 = do	addComment $ "boxing2 " ++ show t
	calc	<- llvmOfXPrim (toLlvmType t) op args
	addComment $ "Erik : " ++ show calc
	boxAny	calc

boxExp t (XVar v1 t1@TCon{})
 = do	addComment $ "boxing3 " ++ show t
	boxAny $ toLlvmVar v1 t1

boxExp t x
 = panic stage $ "Unhandled : boxExp\n    " ++ show t ++ "\n    " ++ take 30 (show x)


--------------------------------------------------------------------------------

-- LLVM does not allow implicit fall through to a label, so explicitly branch
-- to the label immediately following.
branchLabel :: String -> LlvmM ()
branchLabel name
 = do	let label = fakeUnique name
	addBlock [Branch (LMLocalVar label LMLabel), MkLabel label]

--------------------------------------------------------------------------------

llvmOfReturn :: Exp a -> LlvmM ()
llvmOfReturn (XVar v t)
 =	addBlock [ Return (Just (toLlvmVar v t)) ]

llvmOfReturn x
 = 	panic stage $ "llvmOfReturn " ++ (takeWhile (/= ' ') (show x))

--------------------------------------------------------------------------------

primMapFunc
	:: LlvmType
	-> (LlvmVar -> LlvmVar -> LlvmExpression)
	-> LlvmVar
	-> Exp a
	-> LlvmM LlvmVar

primMapFunc t build sofar exp
 = do	val		<- llvmVarOfExp exp
	dst		<- lift $ newUniqueNamedReg "prim.fold" t
	addBlock	[ Assignment dst (build sofar val) ]
	return		dst




llvmOfPtrManip :: LlvmType -> Prim -> [Exp a] -> LlvmM LlvmVar
llvmOfPtrManip t FAdd args
 = case args of
	[l@(XVar v t), XInt i]
	 ->	do	addComment "llvmOfPtrManip"
			src		<- lift $ newUniqueReg $ toLlvmType t
			dst		<- lift $ newUniqueReg $ toLlvmType t
			addBlock	[ Assignment src (loadAddress (toLlvmVar v t))
					, Assignment dst (GetElemPtr True src [llvmWordLitVar i]) ]
			return dst

	_ ->	do	lift $ mapM_ (\a -> putStrLn ("\n    " ++ show a)) args
			panic stage $ "Unhandled : llvmOfPtrManip"

llvmOfPtrManip _ op _
 = panic stage $ "Unhandled : llvmOfPtrManip " ++ show op


llvmOfXPrim :: LlvmType -> Prim -> [Exp a] -> LlvmM LlvmVar
llvmOfXPrim t op args
 = case args of
	[]	-> panic stage "llvmOfXPrim : empty list"
	[x]	-> panic stage "llvmOfXPrim : singleton list"

	x : xs
	 -> do	dst	<- llvmVarOfExp x
		foldM (primMapFunc t (llvmOpOfPrim op)) dst xs



llvmVarOfExp :: Exp a -> LlvmM LlvmVar
llvmVarOfExp (XVar v t@TCon{})
 = do	addComment "llvmVarOfExp (XVar v Int32#)"
	return	$ toLlvmVar v t

llvmVarOfExp (XVar v t)
 = do	reg	<- lift $ newUniqueReg pObj
	addBlock [ Comment ["llvmVarOfExp (XVar v t)"], Assignment reg (Load (toLlvmVar v t)) ]
	return	reg

llvmVarOfExp (XInt i)
 = do	reg	<- lift $ newUniqueReg i32
	addBlock [ Comment ["llvmVarOfExp (XInt i)"], Assignment reg (Load (llvmWordLitVar i)) ]
	return	reg

llvmVarOfExp (XUnbox ty@TCon{} (XVar v t))
 =	unboxAny (toLlvmType ty) (toLlvmVar v t)

llvmVarOfExp (XUnbox ty@TCon{} (XSlot v _ i))
 = do	objptr	<- readSlot i
	unboxAny (toLlvmType ty) objptr

llvmVarOfExp (XUnbox ty@TCon{} (XForce (XSlot _ _ i)))
 = do	orig	<- readSlot i
	forced	<- forceObj orig
	unboxAny (toLlvmType ty) forced

llvmVarOfExp (XUnbox ty@TCon{} (XVarCAF v t))
 =	unboxAny (toLlvmType ty) (pVarLift (toLlvmCafVar v t))


llvmVarOfExp x
 = panic stage $ "llvmVarOfExp " ++ show x




llvmOpOfPrim :: Prim -> (LlvmVar -> LlvmVar -> LlvmExpression)
llvmOpOfPrim p
 = case p of
	FAdd	-> LlvmOp LM_MO_Add
	FSub	-> LlvmOp LM_MO_Sub
	FMul	-> LlvmOp LM_MO_Mul

	FEq	-> Compare LM_CMP_Eq
	_	-> panic stage $ "llvmOpOfPrim : Unhandled op : " ++ show p


-- | Convert a Sea type to an LlvmType.
toLlvmType :: Type -> LlvmType
toLlvmType (TPtr t)	= LMPointer (toLlvmType t)
toLlvmType TObj		= structObj
toLlvmType TVoid	= LMVoid

toLlvmType (TCon v _)
 = case varName v of
	"Bool#"		-> i1
	"Int32#"	-> i32
	"Int64#"	-> i64
	name		-> panic stage $ "toLlvmType unboxed " ++ name ++ "\n"

toLlvmType t		= panic stage $ "toLlvmType " ++ show t ++ "\n"


-- | Convert a Sea Var (wit a Type) to a typed LlvmVar.
toLlvmVar :: Var -> Type -> LlvmVar
toLlvmVar v t
 = case isGlobalVar v of
	True -> LMGlobalVar (seaVar False v) (toLlvmType t) External Nothing (alignOfType t) False
	False -> LMNLocalVar (seaVar True v) (toLlvmType t)

alignOfType :: Type -> Maybe Int
alignOfType (TPtr _) = ptrAlign
alignOfType _ = Nothing

toLlvmCafVar :: Var -> Type -> LlvmVar
toLlvmCafVar v t
 = LMGlobalVar ("_ddcCAF_" ++ seaVar False v) (toLlvmType t) External Nothing Nothing False

toLlvmGlobalFunc :: Var -> Type -> [Exp a] -> LlvmVar
toLlvmGlobalFunc v t args
 =	let	name = seaVar False v
		decl = LlvmFunctionDecl {
			--  Unique identifier of the function
			decName = name,
			--  LinkageType of the function
			funcLinkage = Internal,
			--  The calling convention of the function
			funcCc = CC_Ccc,
			--  Type of the returned value
			decReturnType = toLlvmType t,
			--  Indicates if this function uses varargs
			decVarargs = FixedArgs,
			--  Parameter types and attributes
			decParams = [],     -- [LlvmParameter],
			--  Function align value, must be power of 2
			funcAlign = ptrAlign
			}
		func = LMFunction decl
	in	LMGlobalVar name func Internal Nothing ptrAlign False


-- | Does the given Sea variable have global scope? TODO: Move this to the Sea stuff.
isGlobalVar :: Var -> Bool
isGlobalVar v
 -- If the variable is explicitly set as global use the given name.
 | bool : _	<- [global | ISeaGlobal global <- varInfo v]
 = bool

 | otherwise
 = False

{-
llvmIntLitVar :: LiteralFmt -> LlvmVar
llvmIntLitVar (LiteralFmt (LInt i) (UnboxedBits 32)) = i32LitVar i
llvmIntLitVar (LiteralFmt (LInt i) (UnboxedBits 64)) = i64LitVar i

llvmIntLitVar _ = panic stage $ "llvmIntLitVar : unhandled case."
-}
