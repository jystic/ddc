module DDC.Core.Llvm.Metadata.Tbaa
       ( MDSuper(..)
       , deriveMD 
       , annot 
       , lookup, lookups )
where
import DDC.Llvm.Syntax.Metadata
import DDC.Llvm.Pretty.Metadata         ()
import DDC.Type.Exp
import DDC.Type.Compounds
import DDC.Type.Predicates
import DDC.Type.Collect
import DDC.Type.Env                     (KindEnv)
import DDC.Core.Exp
import DDC.Core.Llvm.Metadata.Graph
import DDC.Core.Llvm.Convert.Base
import DDC.Base.Pretty                  hiding (empty)
import qualified DDC.Type.Env           as Env
import qualified DDC.Core.Salt          as A
import qualified DDC.Llvm.Syntax        as V

import Prelude                          hiding (lookup)
import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Map                         (Map)
import Data.List                        hiding (lookup)
import qualified Data.Map               as Map
import qualified Data.Set               as Set


-- Metadata management --------------------------------------------------------
-- | Metadata for a supercombinator.
data MDSuper
        = MDSuper
          { -- Map bound regions to metadata nodes for attaching metadata 
            --    to relevant instructions.
            nameMap     :: MDEnv
            
            -- Metadata nodes, to be pretty-printed with the module as
            --    declarations. e.g. "!1 = !{!"id", !parent, !i11}
          , decls       :: [MDecl]
          } deriving Show

instance Pretty (MDSuper) where
 ppr (MDSuper _ metadata)
  = vcat $ map ppr metadata


-- | Map region variables to relevant metadata
--      need the whole declaration for tags, e.g "!tbaa", "!debug"
type MDEnv = Map (Bound A.Name) [MDecl]
 
emptyDict :: MDEnv
emptyDict = Map.empty

extendDict :: (Bound A.Name, MDecl) -> MDEnv -> MDEnv
extendDict (u, n) e | Map.member u e = Map.adjust (n:) u e
                    | otherwise      = Map.insert u [n] e


-- | Lookup the metadata for a name, from the metadata tree attached
--   to a supecombinator.
lookup :: Bound A.Name -> MDSuper -> Maybe [MDecl]
lookup u mdsup = Map.lookup u (nameMap mdsup)


-- | Like `lookup` but lookup metadata for several names at once.
lookups :: [Bound A.Name] -> MDSuper -> [Maybe [MDecl]]
lookups us mdsup = map (flip lookup mdsup) us


-- | Generate tbaa metadata for a top-level Salt supercombinator.
deriveMD
      :: (BindStruct (Exp ()))
      => String                 -- ^ Sanitized name of super
      -> Exp () A.Name          -- ^ Super to derive from
      -> ConvertM MDSuper       -- ^ Metadata encoding witness information            

deriveMD nTop xx
  = let 
        regs                 = collectRegsB xx
        (constwits, diswits) = partitionWits $ collectWitsB xx
        arel                 = constructARel   diswits
        domain               = constructANodes regs constwits
        mdDG                 = orientUG $ UG (domain, arel)
        mdTrees              = partitionDG mdDG
    in  foldM (buildMDTree nTop) (MDSuper emptyDict []) mdTrees


buildMDTree :: String -> MDSuper -> Tree ANode -> ConvertM MDSuper
buildMDTree nTop sup tree
 = let tree' = anchor ARoot tree
   in  bfBuild nTop tree' Nothing sup ARoot


bfBuild :: String -> Tree ANode -> Maybe MRef -> MDSuper -> ANode -> ConvertM MDSuper
bfBuild nTop tree parent sup node
 = case parent of
        Nothing        -> do name <- freshRootName nTop
                             bf Nothing $ tbaaRoot name

        Just parentRef -> do name <- freshNodeName nTop (regionU node)
                             bf (Just $ regionU node) $ tbaaNode name parentRef (isConst node)
   where bf u md 
          = do ref          <- liftM MRef $ newUnique
               let sup'     =  declare u ref md sup
               let children =  sources node tree
               foldM (bfBuild nTop tree (Just ref)) sup' children
         declare u r m s 
          = let decl = MDecl r m
            in  case u of Nothing -> s { decls   = decl:(decls s) }
                          Just u' -> s { nameMap = extendDict (u',decl) $ nameMap s
                                       , decls   = decl:(decls s) }


freshNodeName :: String -> Bound A.Name -> ConvertM String
freshNodeName q (UName nm)
    | Just n <- A.takeNameVar nm
    = return $ q ++ "_" ++ n
freshNodeName q _
    = liftA (\i -> q ++ "_" ++ (show i)) newUnique

freshRootName :: String -> ConvertM String
freshRootName qualify = liftA (\i -> qualify ++ "_ROOT_" ++ (show i)) newUnique


-- | Attach relevant metadata to instructions
annot :: (BindStruct c, Show (c A.Name))
      => KindEnv A.Name 
      -> MDSuper        -- ^ Metadata      
      -> [c A.Name]     -- ^ Things to lookup for Meta data.
      -> V.Instr        -- ^ Instruction to annotate
      -> V.AnnotInstr
      
annot kenv mdsup xs ins
 = let regions  = concatMap (collectRegsU kenv) xs
       mdecls   = concat $ catMaybes $ lookups regions mdsup
       annotate' ms is 
         = case is of
                V.ILoad{}  -> V.AnnotInstr is ms
                V.IStore{} -> V.AnnotInstr is ms
                _          -> V.AnnotInstr is []
   in  annotate' mdecls ins


-- Alias relation -------------------------------------------------------------
-- | A node in the alias graphs, representing a region
data ANode  = ANode { regionU :: RegBound
                    , isConst :: Bool }
            | ARoot
              deriving (Show, Eq, Ord)


-- | Make nodes from regions
constructANodes :: [RegBound] -> [WitType] -> [ANode]
constructANodes regs constwits
 = let isConstR r = or $ map (flip isConstWFor r) constwits
       mkANode r  = ANode r (isConstR r)
   in  map mkANode regs


-- | Encode the `alias` relation defined on the set regions
--      * reflexitivity is taken as implicit 
--        (important for generating DAG, and safe since LLVM already assumes
--         reflexitivity)
--      * symmetry is made explicit
--      note that `alias` is non-transitive.
--
constructARel :: [WitType] -> Rel ANode
constructARel diswits = alias
  where alias n1 n2
          | n1 == n2  = False
          | otherwise = not $ or 
                      $ map (flip isDistinctWFor (regionU n1, regionU n2)) diswits


-- Collecting bounds ----------------------------------------------------------
type RegBound  = Bound A.Name
type WitType   = Type A.Name


isConstW :: WitType -> Bool
isConstW t  = isConstWitType t


isConstWFor :: WitType -> RegBound -> Bool
isConstWFor t r
  | _ : args <- takeTApps t
  = and [isConstWitType t, elem (TVar r) args]
  | otherwise = False


isDistinctW :: WitType-> Bool
isDistinctW tw 
  | tc : _ <- takeTApps tw = isDistinctWitType tc
  | otherwise              = False


isDistinctWFor :: WitType -> (RegBound, RegBound) -> Bool
isDistinctWFor t (r1,r2)
  | tc : args <- takeTApps t
  = and [isDistinctWitType tc, (TVar r1) `elem` args, (TVar r2) `elem` args]
  | otherwise = False


-- | Divide a set of witnesses to a set of Const wits and a set of Distinct wits
partitionWits :: [WitType] -> ([WitType], [WitType])
partitionWits ws
  = partition isConstW
  $ filter    (liftA2 (||) isConstW isDistinctW) ws


-- | Collect region bounds
collectRegsU :: (BindStruct c) => KindEnv A.Name -> c A.Name -> [RegBound]
collectRegsU kenv cc
 = let isReg u = case Env.lookup u kenv of
                      Just t | isRegionKind t -> True
                      _                       -> False
   in  filter isReg $ Set.toList (collectBound cc)


-- | Collect region bindings
collectRegsB :: (BindStruct c) => c A.Name -> [RegBound]
collectRegsB cc
 = let isBindReg b 
         = case b of
                BName n t | isRegionKind t -> Just (UName n)
                _                          -> Nothing
       bindRegs = map (isBindReg) $ fst (collectBinds cc)                            
   in  catMaybes bindRegs   

   
-- | Collect witness bindings together with their types (for convinience)
collectWitsB :: (BindStruct c) => c A.Name -> [WitType]
collectWitsB cc
 = let isBindWit b
        = let t = typeOfBind b
          in  if isWitnessType t then Just t else Nothing                     
       bindWits  = map (isBindWit) $ snd (collectBinds cc)
   in  catMaybes bindWits

