
module Core.Util.Strip 
	( stripSchemeT
	, buildScheme
	, slurpForallsT 
	, stripContextT 
	, slurpForallContextT )
where
import DDC.Type

stripSchemeT	:: Type 
		-> 	( [(Bind, Kind)]
			, [Fetter]
			, [Kind] 
			, Type)
			
stripSchemeT tt 
 = stripSchemeT' [] [] [] tt
 
stripSchemeT' forallVTs fsAcc classes tt
 = case tt of
	TForall BNil k tRest
	 -> stripSchemeT' 
	 		forallVTs 
			fsAcc 
			(classes ++ [k])
			tRest
	

 	TForall v t tRest 
	 -> stripSchemeT' 
	 		(forallVTs ++ [(v, t)]) 
	 		fsAcc
			classes 
			tRest

	TFetters tRest fs
	 -> stripSchemeT' 
	 		forallVTs
			(fsAcc ++ fs) 
			classes 
			tRest

	_ ->    ( forallVTs
	     	, fsAcc
		, classes
		, tt)



-----
buildScheme ::	[(Bind, Kind)] -> [Fetter] -> [Kind] -> Type -> Type
buildScheme	forallVTs bindVTs classes shape
 = let	tC	= foldl (\s k	   -> TForall BNil k s)  shape	
		$ reverse classes

	tL	= case bindVTs of
			[]	-> tC
			_	-> TFetters tC bindVTs

 	tF	= foldl (\s (v, t) -> TForall v t s) tL 	
		$ reverse forallVTs

   in	tF


slurpForallsT 
	:: Type -> [(Bind, Kind)]

slurpForallsT tt
 = let 	(forallVTs, _, _, _)	= stripSchemeT tt
   in	forallVTs


-- | slurp of forall bound vars and contexts from the front of this type
slurpForallContextT :: Type -> ([Type], [Kind])
slurpForallContextT tt
 = case tt of
	TForall BNil k1 t2	
	 -> let (vs, ks) = slurpForallContextT t2
	    in	( vs
	        , k1 : ks)

 	TForall b k t2	
	 -> let	tBind	= case b of
	 			BVar v 		-> TVar k $ UVar v
				BMore v t	-> TVar k $ UMore v t
				
		(vs, ks) = slurpForallContextT t2
		
	    in	( tBind : vs
	    	, ks)


	TFetters t1 fs
	 -> slurpForallContextT t1

	_		
	 -> ([], [])


-- | strip context off the front of this type
stripContextT :: Type -> Type
stripContextT tt
 = case tt of
 	TForall BNil k t	-> stripContextT t
	TFetters t fs		-> TFetters (stripContextT t) fs
	_			-> tt




