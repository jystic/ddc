{-# OPTIONS -fwarn-unused-imports #-}

-- | Wrappers for compiler stages dealing with LLVM code.
module Main.Llvm
	(compileViaLlvm)
where

-- main stages
import Main.Setup
import Main.Sea
import Main.Util

import DDC.Main.Error
import DDC.Main.Pretty
import DDC.Var

import qualified Module.Scrape		as M
import qualified DDC.Main.Arg		as Arg
import qualified DDC.Config.Version	as Version

import Llvm
import Llvm.Invoke
import Llvm.GhcReplace.Unique
import Llvm.Runtime
import Llvm.Util

import Sea.Exp
import Sea.Util				(eraseAnnotsTree)
import Sea.Pretty

import Util
import qualified Data.Map		as Map
import qualified Config.Config		as Config

import Control.Monad			(foldM)

import qualified Debug.Trace		as Debug

stage = "Main.Llvm"

debug = False

trace s v
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
	let 	([ 	seaProtos, 		seaSupers
		 , 	seaCafProtos,		seaCafSlots,		seaCafInits
		 ,	seaData
		 , 	seaHashDefs ], junk)

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

	let fwddecls	= [panicOutOfSlots, panicSlotUnderflow]

	let globals	= moduleGlobals
			++ (catMap llvmOfSeaGlobal $ eraseAnnotsTree seaCafSlots)

	decls		<- catMapM llvmOfSeaDecls $ eraseAnnotsTree code

	return $ LlvmModule comments aliases globals fwddecls decls


llvmOfSeaDecls :: Top (Maybe a) -> IO [LlvmFunction]
llvmOfSeaDecls (PCafInit v t ss)
 = panic stage "Implement 'llvmOfSeaDecls (PCafInit v t ss)'"


llvmOfSeaDecls (PSuper v p t ss)
 = do	when debug
	  $ putStrLn "--------------------------------------------------------"
	{-
	putStrLn $ seaVar False v
	putStrLn ""
	mapM_ (\s -> putStrLn $ show s) ss
	putStrLn "--------------------------------------------------------"
	-}
	blocks <- catMapM llvmOfStmt ss
 	return $ [ LlvmFunction
		(LlvmFunctionDecl (seaVar False v) External CC_Ccc (toLlvmType t) FixedArgs (map llvmOfParams p) Nothing)
		(map (seaVar True . fst) p)	-- funcArgs
		[]				-- funcAttrs
		Nothing				-- funcSect
		[ LlvmBlock (fakeUnique "entry") (blocks ++ [ Return (hackReturnVar t) ]) ]	-- funcBody
		]

hackReturnVar :: Type -> Maybe LlvmVar
hackReturnVar t
 = case t of
	TVoid -> Nothing
	TObj -> Just nullObj
	TPtr TObj -> Just nullObj
	_ -> panic stage $ "hackReturnVar " ++ show t

llvmOfParams :: (Var, Type) -> LlvmParameter
llvmOfParams (v, t) = (toLlvmType t, [])


llvmOfSeaGlobal :: Top (Maybe a) -> [LMGlobal]
llvmOfSeaGlobal (PCafSlot v t)
 | t == TObj	-- TODO: This should be 'TPtr (TPtr TObj)'. Fix it upstream.
 =	let	tt = toLlvmType (TPtr (TPtr TObj))
		var = LMGlobalVar
 			("_ddcCAF_" ++ seaVar False v)	-- Variable name
			tt				-- LlvmType
			Internal			-- LlvmLinkageType
			Nothing				-- LMSection
			ptrAlign			-- LMAlign
			False				-- LMConst
	in [(var, Just (LMStaticLit (LMNullLit tt)))]

 | otherwise
 = panic stage $ "llvmOfSeaGlobal on type : " ++ show t

moduleGlobals :: [LMGlobal]
moduleGlobals
 = 	[ ( ddcSlotPtr, Nothing )
	, ( ddcSlotMax, Nothing )
	, ( ddcSlotBase, Nothing ) ]


llvmOfStmt :: Stmt a -> IO [LlvmStatement]
llvmOfStmt stmt
 = case stmt of
	SBlank		-> return $ [Comment [""]]
	SEnter n	-> return $ runtimeEnter n
	SLeave n	-> return $ runtimeLeave n
	SComment s	-> return $ [Comment [s]]
	SGoto loc	-> return $ [Branch (LMNLocalVar (seaVar False loc) LMLabel)]
	SAssign v1 t v2 -> llvmOfAssign v1 t v2

	SAuto v t
	  ->	-- LLVM is SSA so auto variables do not need to be declared.
		return [Comment ["SAuto " ++ seaVar True v ++ " " ++ show t]]

	SLabel l
	  ->	-- LLVM does not allow implicit fall through to a label, so
		-- explicitly branch to the label immediately following.
		let label = fakeUnique (seaVar False l)
		in return $ [Branch (LMLocalVar label LMLabel), MkLabel label]

	SHackery s
	  ->	-- Left over inline C code.
		panic stage $ "SHackery '" ++ s ++ "' not supported in LLVM backend."

	_
	  -> do	when debug
		  $ putStrLn $ take 150 $ show stmt
		return []


--------------------------------------------------------------------------------

llvmOfAssign :: Exp a -> Type -> Exp a -> IO [LlvmStatement]
llvmOfAssign (XVar v1 t1) t (XVar v2 t2)
 | t1 == TPtr (TPtr TObj) && t2 == TPtr (TPtr TObj) && t == TPtr (TPtr TObj)
	&& isGlobalVar v1 && isGlobalVar v2
 = do	tmp	<- newUniqueLocal (toLlvmType t1)
	return	$ [ Assignment tmp (loadAddress (toLlvmVar v2 t2))
		  , Store tmp (pVarLift (toLlvmVar v1 t1)) ]



llvmOfAssign (XVar v1 t1) t x@(XPrim op args)
 | t1 == TPtr (TPtr TObj)
 = do	-- putStrLn $ "\nSAssign (" ++ seaVar False v1 ++ " " ++ show t1 ++ ") " ++ show x ++ ")\n"

	(dstreg, oplist) <- llvmOfXPrim (toLlvmType t) op args
	return	$ reverse oplist ++ [ Store dstreg (pVarLift (toLlvmVar v1 t1)) ]


llvmOfAssign _ _ _
 = return []

--------------------------------------------------------------------------------

primFoldFunc
	:: LlvmType
	-> (LlvmVar -> LlvmVar -> LlvmExpression)
	-> (LlvmVar, [LlvmStatement])
	-> Exp a
	-> IO (LlvmVar, [LlvmStatement])

primFoldFunc t build (left, ss) exp
 = do	dst <- newUniqueLocal t
	return $ (dst, Assignment dst (build left (llvmVarOfExp exp)) : ss)


llvmOfXPrim :: LlvmType -> Prim -> [Exp a] -> IO (LlvmVar, [LlvmStatement])
llvmOfXPrim t op args
 = case args of
	[]			-> panic stage "llvmOfXPrim : empty list"
	[x]			-> panic stage "llvmOfXPrim : singleton list"

	[XVar v t, XInt i]	-> llvmPtrOp v t op i

	x : xs
	  -> do		reg <- newUniqueLocal t
			foldM (primFoldFunc t (llvmOpOfPrim op)) (reg, [Assignment reg (Load (llvmVarOfExp x))]) xs

llvmPtrOp :: Var -> Type -> Prim -> Int -> IO (LlvmVar, [LlvmStatement])
llvmPtrOp v t op i
 = do	src	<- newUniqueLocal (toLlvmType t)
	dst	<- newUniqueLocal (toLlvmType t)
	return	$ (dst, [ Assignment dst (GetElemPtr False src [llvmWordLitVar i])
			, Assignment src (Load (pVarLift (toLlvmVar v t))) ])

llvmVarOfExp :: Exp a -> LlvmVar
llvmVarOfExp x
 = case x of
	XVar v t	-> toLlvmVar v t
	XInt i		-> llvmWordLitVar i
	_ -> panic stage $ "llvmVarOfExp " ++ show x

llvmOpOfPrim :: Prim -> (LlvmVar -> LlvmVar -> LlvmExpression)
llvmOpOfPrim p
 = case p of
	FAdd -> LlvmOp LM_MO_Add
	FSub -> LlvmOp LM_MO_Sub
	_ -> panic stage $ "llvmOpOfPrim : Unhandled op : " ++ show p
