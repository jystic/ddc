
-- | Application of simplifiers to modules and expressions.
module DDC.Core.Simplifier.Apply
        ( applySimplifier
        , applyTransform

        , applySimplifierX
        , applyTransformX)
where
import DDC.Base.Pretty
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Core.Simplifier.Base
import DDC.Core.Transform.AnonymizeX
import DDC.Core.Transform.Snip
import DDC.Core.Transform.Flatten
import DDC.Core.Transform.Beta
import DDC.Core.Transform.Forward
import DDC.Core.Transform.Inline
import DDC.Core.Transform.Namify
import DDC.Core.Transform.Rewrite
import Control.Monad.State.Strict


-- Modules --------------------------------------------------------------------
-- | Apply a simplifier to a module.
--
--   The state monad holds a fresh name generator.
applySimplifier 
        :: (Show a, Ord n, Show n, Pretty n) 
        => Simplifier s a n     -- ^ Simplifier to apply.
        -> Module a n           -- ^ Module to simplify.
        -> State s (Module a n)

applySimplifier spec mm
 = case spec of
        Seq t1 t2
         -> do  mm'     <- applySimplifier t1 mm
                applySimplifier t2 mm'

        Trans t1
         -> applyTransform t1 mm


-- | Apply a transform to a module.
applyTransform
        :: (Show a, Ord n, Show n, Pretty n)
        => Transform s a n      -- ^ Transform to apply.
        -> Module a n           -- ^ Module to simplify.
        -> State s (Module a n)

applyTransform spec mm
 = case spec of
        Id               -> return mm
        Anonymize        -> return $ anonymizeX mm
        Snip             -> return $ snip mm
        Flatten          -> return $ flatten mm
        Beta             -> return $ betaReduce mm
        Forward          -> return $ forwardModule mm
        Namify namK namT -> namifyUnique namK namT mm
        Inline getDef    -> return $ inline getDef mm
        _                -> error "applyTransform: finish me"


-- Expressions ----------------------------------------------------------------
-- | Apply a simplifier to an expression.
--
--   The state monad holds a fresh name generator.
applySimplifierX 
        :: (Show a, Show n, Ord n, Pretty n)
        => Simplifier s a n     -- ^ Simplifier to apply.
        -> Exp a n              -- ^ Exp to simplify.
        -> State s (Exp a n)

applySimplifierX spec xx
 = case spec of
        Seq t1 t2
         -> do  xx'     <- applySimplifierX t1 xx
                applySimplifierX t2 xx'

        Trans t1
         -> applyTransformX  t1 xx


-- | Apply a transform to an expression.
applyTransformX 
        :: (Show a, Show n, Ord n, Pretty n)
        => Transform s a n      -- ^ Transform to apply.
        -> Exp a n              -- ^ Exp  to transform.
        -> State s (Exp a n)

applyTransformX spec xx
 = case spec of
        Id                -> return xx
        Anonymize         -> return $ anonymizeX xx
        Snip              -> return $ snip xx
        Flatten           -> return $ flatten xx
        Inline  getDef    -> return $ inline getDef xx
        Beta              -> return $ betaReduce xx
        Forward           -> return $ forwardX xx
        Namify  namK namT -> namifyUnique namK namT xx
        Rewrite rules     -> return $ fst $ rewrite rules xx

