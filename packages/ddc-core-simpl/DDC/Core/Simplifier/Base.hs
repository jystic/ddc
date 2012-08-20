
module DDC.Core.Simplifier.Base
        ( Simplifier(..)
        , Transform(..))
where
import DDC.Core.Transform.Rewrite
import DDC.Core.Transform.Namify
import DDC.Core.Exp
import DDC.Type.Env
import DDC.Base.Pretty
import Data.Monoid


-- Simplifier -----------------------------------------------------------------
-- | Desription of how to simplify a core program
data Simplifier s a n
        = Seq   (Simplifier s a n) (Simplifier s a n)
        | Trans (Transform s a n)


instance Monoid (Simplifier s a n) where
 mempty  = Trans Id
 mappend = Seq


-- Transform ------------------------------------------------------------------
-- | Represents individual transforms to apply during simplification.
data Transform s a n
        = Id
        | Anonymize
        | Snip
        | Flatten
        | Beta
        | Forward

        | Inline
                { transInlineDef   :: n -> Maybe (Exp a n) }

        | Namify
                { transMkNamifierT :: Env n -> Namifier s n
                , transMkNamifierX :: Env n -> Namifier s n }

        | Rewrite
                { transRules       :: [(String,RewriteRule a n)] }



instance Pretty (Simplifier s a n) where
 ppr ss
  = case ss of
        Seq s1 s2
         -> ppr s1 <+> semi <+> ppr s2

        Trans t1
         -> ppr t1


instance Pretty (Transform s a n) where
 ppr ss
  = case ss of
        Id              -> text "Id"
        Anonymize       -> text "Anonymize"
        Snip            -> text "Snip"
        Flatten         -> text "Flatten"
        Beta            -> text "Beta"
        Forward         -> text "Forward"
        Inline{}        -> text "Inline"
        Namify{}        -> text "Namify"
        Rewrite{}       -> text "Rewrite"

