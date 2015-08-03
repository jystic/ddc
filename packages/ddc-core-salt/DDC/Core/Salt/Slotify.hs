{-# OPTIONS_GHC -w #-}

module DDC.Core.Salt.Slotify
        (slotifyModule)
where

import DDC.Base.Pretty
import DDC.Core.Annot.AnTEC
import DDC.Core.Compounds
import DDC.Core.Exp
import DDC.Core.Module
import DDC.Core.Salt
import DDC.Core.Salt.Compounds
import DDC.Core.Salt.Object
import DDC.Core.Salt.Runtime
import qualified DDC.Core.Check                         as Check
import qualified DDC.Core.Simplifier                    as Simp
import qualified DDC.Core.Simplifier.Recipe             as Simp
import qualified DDC.Core.Transform.Snip                as Snip
import qualified DDC.Core.Transform.Namify              as Namify
import qualified DDC.Core.Transform.SubstituteXX        as Subst
import qualified DDC.Core.Transform.Reannotate          as Reannotate
import qualified DDC.Type.Env                           as Env
import qualified Control.Monad.State.Strict             as State
import Data.Map                                         (Map)
import qualified Data.Map                               as Map
import Data.Set                                         (Set)
import qualified Data.Set                               as Set

---------------------------------------------------------------------------------------------------
-- | Insert slot allocations for heap objects.
slotifyModule
        :: (Show a, Pretty a)
        => a
        -> Module (AnTEC a Name) Name
        -> Either (Error  (AnTEC a Name))
                  (Module (AnTEC a Name) Name)

slotifyModule a mm@ModuleCore{}
        | mmStrip               <- Reannotate.reannotate annotTail mm
        , XLet aa (LRec bxs) x1 <- moduleBody mmStrip
        = let
                bxs'    = map (slotifyLet a) bxs
                mmSlots = mmStrip { moduleBody = XLet aa (LRec bxs') x1 }

                anorm   = Simp.anormalize (Namify.makeNamifier freshT)
                                          (Namify.makeNamifier freshX)

                mmANF   = Simp.result $ fst
                        $ flip State.runState 0
                        $ Simp.applySimplifier
                                profile Env.empty Env.empty anorm mmSlots

          in    case Check.checkModule (Check.configOfProfile profile) mmANF Check.Recon of

                -- Couldn't reconstruct type annotations.
                (Left err, _checkTrace)
                  -> error ("slotifyModule:\n" ++ renderIndent (ppr err)) -- TODO how to report error properly

                (Right mmCheck, _checkTrace)
                  -> Right mmCheck

        | otherwise
        = Left (ErrorNoTopLevelLetrec mm)


-- Top level let bindings ------------------------------------------------------
slotifyLet
        :: a
        -> (Bind Name, Exp a Name)
        -> (Bind Name, Exp a Name)

slotifyLet a (BName n t, x)
 = (BName n t, slotifySuper a x)

slotifyLet a bx
 = bx


-- Super -----------------------------------------------------------------------
slotifySuper
        :: a
        -> Exp a Name
        -> Exp a Name

slotifySuper a xx
 = let  objs            = objectsOfExp xx
        objs'           = Map.toList objs

        nSlot n         = NameExt n "slot"
        xSlot n         = XVar a (UName (nSlot n))
        tSlot t         = tPtr rTop t
        bSlot n t       = BName (nSlot n) (tSlot t)

        xPeekSlot  n t  = xPeek a rTop t (xSlot n) 0
        xPokeSlot  n t  = xPoke a rTop t (xSlot n) 0 (XVar a (UName n))
        xPokeSlot' n t  = XLet a (LLet (BNone tVoid) (xPokeSlot n t))

        allocs          = [ XLet a (LLet (bSlot n t) (xAllocSlot a tR))
                          | (n, t)              <- objs'
                          , Just (tR, _)        <- [takeTPtr t] ]

        allocSlots x    = foldr ($) x allocs

        peeks           = [ (BName n t, xPeekSlot n t)
                          | (n, t)              <- objs' ]

        substPeeks x    = Subst.substituteXXs peeks x

        pokes           = Map.fromList
                          [ (n, xPokeSlot' n t)
                          | (n, t)              <- objs' ]

        injectPokesL x  = injectX pokes x

   in  case takeXLamFlags xx of
         Nothing
          -> allocSlots $ injectPokesL $ substPeeks xx

         Just (bs, xx')
          -> let
                -- Get level-0 binders
                args            = Map.fromList [ (n, ()) | (False, BName n _) <- bs ]

                pokes           = [ xPokeSlot' n t
                                  | (n, t)      <- Map.toList (objs `Map.intersection` args) ]

                injectPokesA x  = foldr ($) x pokes

             in makeXLamFlags a bs $ allocSlots
                                   $ injectPokesA
                                   $ injectPokesL
                                   $ substPeeks xx'


---------------------------------------------------------------------------------------------------
-- Inject a code transformation just after a name is bound

injectX :: Map Name (Exp a Name -> Exp a Name)
        -> Exp a Name
        -> Exp a Name

injectX injs xx
 = case xx of
        XVar{}          -> xx
        XCon{}          -> xx
        XLAM  a b x     -> XLAM  a b   (injectX injs x) -- Should we error? Salt
        XLam  a b x     -> XLam  a b   (injectX injs x) -- doesn't have lambdas.
        XApp  a x1 x2   -> XApp  a     (injectX injs x1)          (injectX injs x2)
        XLet  a lts x   -> XLet  a lts (injectionsOfLets injs lts (injectX injs x))
        XCase a x alts  -> XCase a     (injectX injs x)      (map (injectA injs) alts)
        XCast a c x     -> XCast a c   (injectX injs x)
        XType{}         -> xx
        XWitness{}      -> xx


injectA :: Map Name (Exp a Name -> Exp a Name)
        -> Alt a Name
        -> Alt a Name

injectA injs (AAlt pp xx)
 = AAlt pp (injectionsOfPat injs pp (injectX injs xx))

---------------------------------------------------------------------------------------------------
-- Construct the transformation to inject, given a set of names

injectionsOfLets :: Map Name (exp -> exp) -> Lets a Name -> exp -> exp
injectionsOfLets injs lts = injectionsOfBinds injs (valwitBindsOfLets lts)

injectionsOfPat :: Map Name (exp -> exp) -> Pat Name -> exp -> exp
injectionsOfPat injs pp = injectionsOfBinds injs (bindsOfPat pp)

injectionsOfBinds :: Map Name (exp -> exp) -> [Bind Name] -> exp -> exp
injectionsOfBinds injs binds
 = let
        names   = Map.fromList [(n, ()) | BName n _ <- binds]
        matches = injs `Map.intersection` names
   in
        Map.foldr (.) id matches
