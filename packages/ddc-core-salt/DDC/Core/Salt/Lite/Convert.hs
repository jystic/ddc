
-- | Conversion of Disciple Lite to Disciple Salt.
--
module DDC.Core.Salt.Lite.Convert
        ( toSalt
        , Error(..))
where
import DDC.Core.Salt.Lite.Convert.Data
import DDC.Core.Salt.Lite.Convert.Type
import DDC.Core.Salt.Lite.Convert.Base
import DDC.Core.Salt.Platform
import DDC.Core.Module
import DDC.Core.Compounds
import DDC.Core.Predicates
import DDC.Core.Exp
import DDC.Type.Compounds
import DDC.Type.Universe
import DDC.Type.DataDef
import DDC.Type.Check.Monad                     (throw, result)
import DDC.Core.Check                           (AnTEC(..))
import qualified DDC.Core.Salt.Lite.Name        as L
import qualified DDC.Core.Salt.Output.Runtime   as O
import qualified DDC.Core.Salt.Output.Name      as O
import qualified DDC.Core.Salt.Output.Env       as O
import qualified Data.Map                       as Map
import Control.Monad


-- | Convert a Disciple Core Lite module to Disciple Core Salt.
--
--   Case expressions on alrebraic data values are converted into ones that just
--   check the tag, while data constructors are unfolded into explicit allocation
--   and field initialization primops. 
--
--   TODO: Add the alternatives that force and follow lazy thunks and indirections.
--   TODO: Expand partial and over-applications into code that explicitly builds
--         and applies thunks.
--
--   The input module needs to be:
--      well typed,
--      have type annotations on every bound variable and constructor
--      have all functions defined at top-level,
--      be fully named,
--      and a-normalised.
--      If not then `Error`.
--
--   The output code contains:
--      debruijn indices.
--       these which need to be eliminateed before it will pass the Salt fragment checks.
--
toSalt
        :: Show a
        => Platform
        -> DataDefs L.Name
        -> Module (AnTEC a L.Name) L.Name 
        -> Either (Error a) (Module a O.Name)
toSalt pp defs mm
 = result $ convertM pp defs mm



-- Module ---------------------------------------------------------------------
convertM 
        :: Show a
        => Platform
        -> DataDefs L.Name 
        -> Module (AnTEC a L.Name) L.Name 
        -> ConvertM a (Module a O.Name)

convertM pp defsPrim mm
  = do  let defs = defsPrim
        x'       <- convertBodyX pp defs $ moduleBody mm

        return $ ModuleCore
         { moduleName           = moduleName mm
         , moduleExportKinds    = Map.empty
         , moduleExportTypes    = Map.empty
         , moduleImportKinds    = Map.empty
         , moduleImportTypes    = O.runtimeImportSigs 
         , moduleBody           = x' }


-- Exp -------------------------------------------------------------------------
convertBodyX 
        :: Show a 
        => Platform
        -> DataDefs L.Name 
        -> Exp (AnTEC a L.Name) L.Name 
        -> ConvertM a (Exp a O.Name)

convertBodyX pp defs xx
 = case xx of
        -- TODO: check there are no debruijn indices.

        XVar a u
         -> do  let a'  = annotTail a
                u'      <- convertU u
                return  $  O.xReturn a' (typeOfBound u') (XVar a' u')

        XCon a u
         -> do  let a'  = annotTail a
                (xx', _) <- convertC defs a' u
                return  xx'

        -- Strip out type lambdas.
        XLAM _ _ x
         -> convertBodyX pp defs x

        -- Keep value binders but ditch witness binders for now.
        XLam a b x
         -> case universeFromType1 (typeOfBind b) of
             Just UniverseData    
              -> liftM3 XLam 
                        (return $ annotTail a) 
                        (convertB b) 
                        (convertBodyX pp defs x)

             Just UniverseWitness 
              -> convertBodyX pp defs x

             _          -> error "toSaltX: unexpected binder" -- throw ErrorMistyped

        XApp{}
         -> do  x'      <- convertArgX pp defs xx
                return  $ x'

        XLet a (LRec bxs) x2
         -> do  let (bs, xs)    = unzip bxs
                bs'     <- mapM convertB bs
                xs'     <- mapM (convertBodyX pp defs) xs
                x2'     <- convertBodyX pp defs x2
                return $ XLet (annotTail a) (LRec $ zip bs' xs') x2'

        XLet a (LLet LetStrict b x1) x2
         -> do  b'      <- convertB b
                x1'     <- convertArgX  pp defs x1
                x2'     <- convertBodyX pp defs x2
                return  $ XLet (annotTail a) (LLet LetStrict b' x1') x2'

        XLet{} 
         -> error "toSaltX: XLEt"

        XCase (AnTEC t _ _ a') x@(XVar _ uX) alts  
         -> do  x'      <- convertArgX pp defs x
                t'      <- convertT t
                alts'   <- mapM (convertA pp defs a' uX) alts

                let asDefault
                        | any isPDefault [p | AAlt p _ <- alts]   
                        = []

                        | otherwise     
                        = [AAlt PDefault (O.xFail a' t')]

                return  $ XCase a' (XApp a' (O.xGetTag a') x') 
                        $ alts' ++ asDefault

        XCase{}         -> throw $ ErrorNotNormalized

        XCast _ _ x     -> convertBodyX pp defs x

        XType _         -> throw $ ErrorMistyped xx
        XWitness{}      -> throw $ ErrorMistyped xx



-- | Convert a function argument or let binding RHS.
convertArgX
        :: Show a 
        => Platform
        -> DataDefs L.Name
        -> Exp (AnTEC a L.Name) L.Name
        -> ConvertM a (Exp a O.Name)

convertArgX pp defs xx
  = case xx of
        -- TODO: check there are no debruijn indices.
        XVar a u        
         -> liftM2 XVar (return $ annotTail a) (convertU u)

        XCon a u
         -> case u of
                UName nCtor _   -> convertCtorAppX pp (annotTail a) defs nCtor []
                UPrim nCtor _   -> convertCtorAppX pp (annotTail a) defs nCtor []
                _               -> throw $ ErrorInvalidBound u


        -- Primitive operations.
        XApp a _ _
         | x1 : xsArgs          <- takeXApps xx
         , XVar _ UPrim{}       <- x1
         -> do  x1'     <- convertArgX pp defs x1
                xsArgs' <- mapM (convertArgX pp defs) xsArgs
                return  $ makeXApps (annotTail a) x1' xsArgs'

        -- Primitive data constructors.
        XApp (AnTEC _t _ _ a') _ _
         | x1 : xsArgs            <- takeXApps xx
         , XCon _ (UPrim nCtor _) <- x1
         -> convertCtorAppX pp a' defs nCtor xsArgs

        XApp a _ _
         | x1 : xsArgs            <- takeXApps xx
         , XCon _ (UName nCtor _) <- x1
         -> convertCtorAppX pp (annotTail a) defs nCtor xsArgs

        XApp (AnTEC _t _ _ _a') _ _
         | x1 : _xsArgs          <- takeXApps xx
         -> error $ "toSaltX: XApp " ++ show x1


        XApp{}          
         -> error $ "toSaltX convertArg: " ++ show xx


        XCast _ _ x     -> convertArgX pp defs x

        -- Lambdas, should have been split out to top-level bindintg.
        XLAM{}          -> throw ErrorNotNormalized
        XLam{}          -> throw ErrorNotNormalized

        -- Lets and cases should 
        XLet{}          -> throw ErrorNotNormalized
        XCase{}         -> throw ErrorNotNormalized 

        -- Types and witness arguments should have been discarded already.
        XType t         -> liftM XType (convertT t)
        XWitness{}      -> error "Xwitness as arg" -- throw ErrorMistyped


convertCtorAppX 
        :: Show a
        => Platform 
        -> a
        -> DataDefs L.Name
        -> L.Name
        -> [Exp (AnTEC a L.Name) L.Name]
        -> ConvertM a (Exp a O.Name)

convertCtorAppX pp a defs nCtor xsArgs
 | Just ctorDef   <- Map.lookup nCtor $ dataDefsCtors defs
 , Just dataDef   <- Map.lookup (dataCtorTypeName ctorDef) $ dataDefsTypes defs
 = do   xs'     <- mapM (convertArgX pp defs) xsArgs
        constructData pp a dataDef ctorDef xs'

convertCtorAppX _ _ _ _ _
 = error "toSaltX: convertCtorAppX failed"


-- Alt ------------------------------------------------------------------------
convertA 
        :: Show a
        => Platform
        -> DataDefs L.Name 
        -> a
        -> Bound L.Name 
        -> Alt (AnTEC a L.Name) L.Name 
        -> ConvertM a (Alt a O.Name)

convertA pp defs a uScrut alt
 = case alt of
        AAlt PDefault x
         -> do  x'      <- convertBodyX pp defs x
                return  $ AAlt PDefault x'


        AAlt (PData uCtor bsFields) x
         | Just nCtor    <- case uCtor of
                                UName n _ -> Just n
                                UPrim n _ -> Just n
                                _         -> Nothing
         , Just ctorDef   <- Map.lookup nCtor $ dataDefsCtors defs
         -> do  
                uScrut'         <- convertU uScrut

                -- Get the tag of this alternative.
                let iTag        = fromIntegral $ dataCtorTag ctorDef
                let uTag        = UPrim (O.NameTag iTag) O.tTag

                -- Get the address of the payload.
                bsFields'       <- mapM convertB bsFields

                -- Convert the right of the alternative.
                xBody1          <- convertBodyX pp defs x

                -- Add let bindings to unpack the constructor.
                xBody2          <- destructData pp a uScrut' ctorDef bsFields' xBody1

                return  $ AAlt (PData uTag []) xBody2


        AAlt{}          -> throw ErrorInvalidAlt



-- Data Constructor -----------------------------------------------------------
convertC :: Show a
         => DataDefs L.Name
         -> a 
         -> Bound L.Name 
         -> ConvertM a (Exp a O.Name, Type O.Name)

convertC _defs a uu
 = case uu of
        UPrim (L.NameInt i bits) _   
          -> return ( XCon a (UPrim (O.NameInt i bits) (O.tInt bits))
                    , O.tInt bits)

        -- TODO: expand out code to construct algebraic data.
        _ -> error $ show uu

