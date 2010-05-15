{-# OPTIONS -fwarn-incomplete-patterns #-}

-- | Functions to sink classids, by themselves or within types and kinds.
--	Sinking is an easier word for canonicalisation.
--	We export plain IO versions, because there are several modules that use them, 
--	each with their own particular state monads.
--
--	This is a fairly common operation during type inference, so we don't want
--	to rely on the boilerplate library (too slow).
--
module DDC.Solve.Sink
	( sinkCidIO
	, sinkCidsInNodeIO
	, sinkCidsInKindIO
	, sinkCidsInTypeIO 
	, sinkCidsInFetterIO)
where
import Type.Exp
import Type.Base
import DDC.Main.Error
import Data.Array.IO
import Control.Monad
import qualified Data.Set	as Set
import qualified Data.Map	as Map

stage = "DDC.Solve.Sink"

-- | Canonicalise a single cid.
sinkCidIO 
	:: IOArray ClassId Class 	-- ^ The type graph.
	-> ClassId 			-- ^ cid to start with.
	-> IO ClassId			-- ^ canonical cid.

sinkCidIO classes cid'
 = go cid'
 where	go cid
	 = do	cls		<- readArray classes cid
		case cls of
		 ClassUnallocated{}	-> panic stage "sinkCidIO: unallocated cass"
		 ClassForward _ cid'	-> go cid'
		 ClassFetter{}		-> return cid
		 ClassFetterDeleted{}	-> return cid
		 Class{}		-> return cid


-- | Canonicalise the cids in a node type.
sinkCidsInNodeIO
	:: IOArray ClassId Class
	-> Node
	-> IO Node
	
sinkCidsInNodeIO classes nn
 = case nn of
	NBot{}		-> return nn
	NVar{}		-> return nn
	NCon{}		-> return nn

	NApp cid1 cid2
	 -> do	cid1'	<- sinkCidIO classes cid1
		cid2'	<- sinkCidIO classes cid2
		return	$ NApp cid1' cid2'

	NSum cids
	 -> do	cids'	<- liftM Set.fromList 
			$  mapM (sinkCidIO classes) 
			$  Set.toList cids
			
		return	$ NSum cids'
		
	NScheme t
	 -> do	t'	<- sinkCidsInTypeIO classes t
		return	$ NScheme t'
		
	NFree v t
	 -> do	t'	<- sinkCidsInTypeIO classes t
		return	$ NFree v t'
		
	NError{}	-> return nn


-- | Canonicalise the cids in a kind.
sinkCidsInKindIO
	:: IOArray ClassId Class
	-> Kind
	-> IO Kind
	
sinkCidsInKindIO classes kk'
 = goK kk'
 where goK kk
 	= case kk of
		KNil		-> return kk
		KCon{}		-> return kk

		KFun k1 k2
		 -> do	k1'	<- goK k1
			k2'	<- goK k2
			return	$ KFun k1' k2'

		KApp k t
		 -> do	k'	<- goK k
			t'	<- sinkCidsInTypeIO classes t
			return	$ KApp k' t'
			
		KSum ks
		 -> do	ks'	<- mapM goK ks
			return	$ KSum ks'
			

-- | Canonicalise the cids in a type.
sinkCidsInTypeIO 
	:: IOArray ClassId Class	-- ^ The type graph.
	-> Type				-- ^ The type to refresh.
	-> IO Type			-- ^ Same type with all cids in canonical form.

sinkCidsInTypeIO classes tt'
 = goT tt'
 where	goF ff 
 	 = sinkCidsInFetterIO classes ff

	goB bb
	 = case bb of
		BNil		-> return bb
		BVar{}		-> return bb
		BMore v t
		 -> do	t'	<- goT t
			return	$ BMore v t'
			
	goCRS crs
	 = do	let goTwo (t1, t2)
			= do	t1'	<- goT t1
				t2'	<- goT t2
				return	(t1', t2')
				
		-- NOTE: We need to go via lists here because the Map keys may change.
		crsEq'		<- liftM Map.fromList $ mapM goTwo $ Map.toList $ crsEq crs
	 	crsMore'	<- liftM Map.fromList $ mapM goTwo $ Map.toList $ crsMore  crs
		crsOther'	<- mapM goF $ crsOther crs
		return 		$ Constraints crsEq' crsMore' crsOther'

	goT tt
	 = case tt of
		TNil		-> return tt
		TVar{}		-> return tt
		TIndex{}	-> return tt
		TCon{}		-> return tt

		TClass k cid
		 -> do	k'	<- sinkCidsInKindIO classes k
			cid'	<- sinkCidIO classes cid
			return	$ TClass k' cid'

		TVarMore k v t
		 -> do	k'	<- sinkCidsInKindIO classes k
			t'	<- goT t
			return	$ TVarMore k' v t'
			
		TSum k ts
		 -> do	k'	<- sinkCidsInKindIO classes k
			ts'	<- mapM goT ts
			return	$ TSum k' ts'
			
		TApp t1 t2
		 -> do	t1'	<- goT t1
			t2'	<- goT t2
			return	$ TApp t1' t2'
			
		TForall b k t
		 -> do	b'	<- goB b
			k'	<- sinkCidsInKindIO classes k
			t'	<- goT t
			return	$ TForall b' k' t'
			
		TFetters t fs
		 -> do	t'	<- goT t
			fs'	<- mapM goF fs
			return	$ TFetters t' fs'
			
		TConstrain t crs
		 -> do	t'	<- goT t
			crs'	<- goCRS crs
			return	$ TConstrain t crs
	
		TError k err
		 -> do	k'	<- sinkCidsInKindIO classes k
			return	$ TError k' err
			
		TElaborate elab t
		 -> do	t'	<- goT t
			return	$ TElaborate elab t'
			
		TFree v t
		 -> do	t'	<- goT t
			return	$ TFree v t'
			
		TDanger t1 t2
		 -> do	t1'	<- goT t1
			t2'	<- goT t2
			return	$ TDanger t1' t2'


-- | Canonicalise the cids in a fetter.
sinkCidsInFetterIO 
	:: IOArray ClassId Class	-- ^ The type graph.
	-> Fetter			-- ^ The type to refresh.
	-> IO Fetter			-- ^ Same type with all cids in canonical form.

sinkCidsInFetterIO classes tt'
 = goF tt'
 where	goT t
	 = sinkCidsInTypeIO classes t
	
	goF ff
	 = case ff of
		FConstraint v ts	
		 -> do	ts'	<- mapM goT ts
			return	$ FConstraint v ts'
			
		FWhere t1 t2
		 -> do	t1'	<- goT t1
			t2'	<- goT t2
			return	$ FWhere t1' t2'
			
		FMore t1 t2
		 -> do	t1'	<- goT t1
			t2'	<- goT t2
			return	$ FMore t1' t2'
			
		FProj j v t1 t2
		 -> do	t1'	<- goT t1
			t2'	<- goT t2
			return	$ FProj j v t1' t2'
			
