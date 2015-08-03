
module DDC.Core.Salt.Object
        (objectsOfExp)
where

import DDC.Core.Exp
import DDC.Core.Salt
import DDC.Core.Salt.Compounds
import Data.Map                 (Map)
import qualified Data.Map       as Map

-- Exp ------------------------------------------------------------------------
objectsOfExp
        :: Exp a Name
        -> Map Name (Type Name)

objectsOfExp xx
 = case xx of
        XVar  _ _       -> Map.empty
        XCon  _ _       -> Map.empty
        XLAM  _ _ x     -> objectsOfExp x
        XLam  _ b x     -> Map.union  (objectsOfBind b)   (objectsOfExp x)
        XApp  _ x1 x2   -> Map.union  (objectsOfExp x1)   (objectsOfExp x2)
        XLet  _ lts x   -> Map.union  (objectsOfLets lts) (objectsOfExp x)
        XCase _ x alts  -> Map.unions (objectsOfExp x : map objectsOfAlt alts)
        XCast _ _ x     -> objectsOfExp x
        XType{}         -> Map.empty
        XWitness{}      -> Map.empty

-- Let ------------------------------------------------------------------------
objectsOfLets
        :: Lets a Name
        -> Map Name (Type Name)

objectsOfLets lts
 = case lts of
        LLet b x        -> Map.union (objectsOfBind b) (objectsOfExp x)
        LRec bxs        -> Map.unions [Map.union (objectsOfBind b) (objectsOfExp x) | (b, x) <- bxs]
        LPrivate{}      -> Map.empty
        LWithRegion{}   -> Map.empty


-- Alt ------------------------------------------------------------------------
objectsOfAlt
        :: Alt a Name
        -> Map Name (Type Name)

objectsOfAlt aa
 = case aa of
        AAlt p x        -> Map.union (objectsOfPat p) (objectsOfExp x)


-- Alt ------------------------------------------------------------------------
objectsOfPat
        :: Pat Name
        -> Map Name (Type Name)

objectsOfPat pp
 = case pp of
        PDefault        -> Map.empty
        PData _ bs      -> Map.unions (map objectsOfBind bs)


-- Bind -----------------------------------------------------------------------
objectsOfBind
        :: Bind Name
        -> Map Name (Type Name)

objectsOfBind bb
 = case bb of
        BNone _
         -> Map.empty

        BAnon t
         | isHeapObject t
         -> error "objectsOfBind: found anonymous heap object"
         -- TODO how to report this error correctly

         | otherwise
         -> Map.empty

        BName n t
         | isHeapObject t
         -> Map.singleton n t

         | otherwise
         -> Map.empty


-- Utils ----------------------------------------------------------------------
-- | Checks if we have a `Ptr# r Obj`.
isHeapObject :: Type Name -> Bool
isHeapObject t
 = case takeTPtr t of
        Nothing      -> False
        Just (_, tp) -> tp == tObj
