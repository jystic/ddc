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
			++ (catMap seaToLlvmGlobal $ eraseAnnotsTree seaCafSlots)

	decls		<- catMapM seaToLlvmDecls $ eraseAnnotsTree code

	return $ LlvmModule comments aliases globals fwddecls decls


seaToLlvmDecls :: Top (Maybe a) -> IO [LlvmFunction]
seaToLlvmDecls (PCafInit v t ss)
 = panic stage "Implement  'seaToLlvmDecls (PCafInit v t ss)'"


seaToLlvmDecls (PSuper v p t ss)
 = do	when debug
	  $ putStrLn "--------------------------------------------------------"
	{-
	putStrLn $ seaVar False v
	putStrLn ""
	mapM_ (\s -> putStrLn $ show s) ss
	putStrLn "--------------------------------------------------------"
	-}
	blocks <- catMapM seaToLlvm ss
 	return $ [ LlvmFunction
		(LlvmFunctionDecl (seaVar False v) External CC_Ccc (toLlvmType t) FixedArgs (map toParams p) Nothing)
		(map (seaVar True . fst) p)	-- funcArgs
		[]				-- funcAttrs
		Nothing				-- funcSect
		[ LlvmBlock (fakeUnique "entry") (blocks ++ [ Return Nothing ]) ]	-- funcBody
		]


toParams :: (Var, Type) -> LlvmParameter
toParams (v, t) = (toLlvmType t, [])


seaToLlvmGlobal :: Top (Maybe a) -> [LMGlobal]
seaToLlvmGlobal (PCafSlot v t)
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
 = panic stage $ "seaToLlvmGlobal on type : " ++ show t

moduleGlobals :: [LMGlobal]
moduleGlobals
 = 	[ ( ddcSlotPtr, Nothing )
	, ( ddcSlotMax, Nothing )
	, ( ddcSlotBase, Nothing ) ]


seaToLlvm :: Stmt a -> IO [LlvmStatement]

seaToLlvm (SBlank)	= return $ [Comment [""]]
seaToLlvm (SEnter n)	= return $ runtimeEnter n
seaToLlvm (SLeave n)	= return $ runtimeLeave n
seaToLlvm (SComment s)	= return $ [Comment [s]]

seaToLlvm (SGoto var)
 =	return $ [Branch (LMNLocalVar (seaVar False var) LMLabel)]

seaToLlvm (SAuto v t)
 =	-- LLVM is SSA so auto variables do not need to be declared.
	return [Comment ["SAuto " ++ seaVar True v ++ " " ++ show t]]

seaToLlvm (SLabel l)
 =	-- LLVM does not allow implicit fall through to a label, so
	-- explicitly branch to the label immediately following.
	let label = fakeUnique (seaVar False l)
	in return $ [Branch (LMLocalVar label LMLabel), MkLabel label]

seaToLlvm (SHackery s)
 =	-- Left over inline C code.
	panic stage $ "SHakery '" ++ s ++ "' not supported in LLVM backend."



seaToLlvm (SAssign (XVar v1 t1) t (XVar v2 t2))
 | t1 == TPtr (TPtr TObj) && t2 == TPtr (TPtr TObj) && t == TPtr (TPtr TObj)
	&& isGlobalVar v1 && isGlobalVar v2
 = do	tmp	<- newUniqueLocal (toLlvmType t1)
	return	$ [ Assignment tmp (loadAddress (toLlvmVar v2 t2))
		  , Store tmp (pVarLift (toLlvmVar v1 t1)) ]


{-=
seaToLlvm (SAssign (XVar v1 t1) t x@XPrim{})
 | t1 == TPtr (TPtr TObj)
 = do	putStrLn $ "\nSAssign (" ++ seaVar False v1 ++ " " ++ show t1 ++ ") " ++ show x ++ ")\n"
	reg	<- newUniqueLocal (toLlvmType t1)
	putStrLn $ show reg
	xprim	<- xprimToLlvm reg t x
	return	$ [ Store reg (pVarLift (toLlvmVar v1 t1)) ]
=-}

seaToLlvm stmt
 = do	when debug
	  $ putStrLn $ take 150 $ show stmt
	return []

{-=
xprimToLlvm reg t (XPrim op args)
 = panic stage "xprimToLlvm"
=-}



