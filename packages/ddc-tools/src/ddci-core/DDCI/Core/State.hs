
module DDCI.Core.State
        ( State         (..)
        , Bundle        (..)
        , initState
        , Mode          (..)
        , adjustMode

        , TransHistory  (..)

        , Source        (..)

        , Language      (..)
        , languages

        -- Driver config.
        , getDriverConfigOfState
        , getDefaultBuilderConfig
        , getActiveBuilder)
where
import DDCI.Core.Mode
import DDC.Code.Config
import DDC.Interface.Input
import DDC.Interface.Source
import DDC.Build.Builder
import DDC.Build.Language
import DDC.Core.Exp
import DDC.Core.Module
import DDC.Core.Simplifier
import DDC.Base.Pretty                          hiding ((</>))
import DDC.Base.Name
import Data.Typeable
import System.FilePath
import Data.Map                                 (Map)
import Data.Set                                 (Set)
import DDC.Core.Check                           (AnTEC(..))
import qualified DDC.Build.Language.Eval        as Eval
import qualified DDC.Core.Salt                  as Salt
import qualified DDC.Core.Simplifier            as S
import qualified DDC.Core.Salt.Runtime          as Runtime
import qualified DDC.Driver.Stage               as D
import qualified DDC.Driver.Config              as D
import qualified Data.Map                       as Map
import qualified Data.Set                       as Set


-- | Interpreter state.
--   This is adjusted by interpreter commands.
data State
        = State
        { -- | ddci interface state.
          stateInterface        :: InputInterface

          -- | ddci mode flags.
        , stateModes            :: Set Mode 

          -- | Source language to accept.
        , stateLanguage         :: Language

          -- | Maps of modules we can use as inliner templates.
        , stateWithSalt         :: Map ModuleName (Module (AnTEC () Salt.Name) Salt.Name)

          -- | Simplifier to apply to core program.
        , stateSimplSalt        :: Simplifier Int () Salt.Name

          -- | Force the builder to this one, this sets the address width etc.
          --   If Nothing then query the host system for the default builder.
        , stateBuilder          :: Maybe Builder

          -- | Output file for @compile@ and @make@ commands.
        , stateOutputFile       :: Maybe FilePath 

          -- | Output dir for @compile@ and @make@ commands
        , stateOutputDir        :: Maybe FilePath

          -- | Interactive transform mode
        , stateTransInteract    :: Maybe TransHistory}



data TransHistory
        = forall s n err
        .  (Typeable n, Ord n, Show n, Pretty n, CompoundName n)
        => TransHistory
        { -- | Original expression and its types
          historyExp            :: (Exp (AnTEC () n) n, Type n, Effect n, Closure n) 

          -- | Keep history of steps so we can go back and construct final sequence
        , historySteps          :: [(Exp (AnTEC () n) n, Simplifier s (AnTEC () n) n)]

          -- | Bundle for the language that we're transforming.
        , historyBundle         :: Bundle s n err }


-- | Adjust a mode setting in the state.
adjustMode 
        :: Bool         -- ^ Whether to enable or disable the mode.        
        -> Mode         -- ^ Mode to adjust.
        -> State
        -> State

adjustMode True mode state
        = state { stateModes    = Set.insert mode (stateModes state) }

adjustMode False mode state
        = state { stateModes    = Set.delete mode (stateModes state) }


-- | The initial state.
initState :: InputInterface -> State
initState interface
        = State
        { stateInterface        = interface
        , stateModes            = Set.empty 
        , stateLanguage         = Eval.language
        , stateWithSalt         = Map.empty
        , stateSimplSalt        = S.Trans S.Id
        , stateBuilder          = Nothing  
        , stateOutputFile       = Nothing
        , stateOutputDir        = Nothing
        , stateTransInteract    = Nothing }


-- | Slurp out the relevant parts of the DDCI stage into a driver config.
getDriverConfigOfState :: State -> IO D.Config
getDriverConfigOfState state
 = do   builder <- getActiveBuilder state
        return 
         $ D.Config
         { D.configLogBuild               = True
         , D.configDump                   = Set.member Dump  (stateModes state)
         , D.configInferTypes             = Set.member Synth (stateModes state)
         , D.configViaBackend             = D.ViaLLVM

         -- ISSUE #300: Allow the default heap size to be set when
         --   compiling the program.
         , D.configRuntime
                = Runtime.Config
                { Runtime.configHeapSize        = 1000000
                , Runtime.configSlotStackSize   = 100000 }

         , D.configRuntimeLinkStrategy    = D.LinkDefault
         , D.configModuleBaseDirectories  = []
         , D.configOutputFile             = stateOutputFile state
         , D.configOutputDir              = stateOutputDir  state
         , D.configSimplSalt              = stateSimplSalt  state
         , D.configBuilder                = builder
         , D.configPretty                 = configPretty
         , D.configSuppressHashImports    = not $ Set.member SaltPrelude (stateModes state) 
         , D.configKeepLlvmFiles          = False
         , D.configKeepSeaFiles           = False
         , D.configKeepAsmFiles           = False 

         , D.configTaintAvoidTypeChecks 
                = Set.member TaintAvoidTypeChecks (stateModes state) }

 where  modes   = stateModes state
        
        configPretty   
         = D.ConfigPretty
         { D.configPrettyVarTypes         = Set.member PrettyVarTypes   modes
         , D.configPrettyConTypes         = Set.member PrettyConTypes   modes
         , D.configPrettyUseLetCase       = Set.member PrettyUseLetCase modes 
         , D.configPrettySuppressImports  = Set.member SuppressImports  modes
         , D.configPrettySuppressExports  = Set.member SuppressExports  modes
         , D.configPrettySuppressLetTypes = Set.member SuppressLetTypes modes }


-- | Holds platform independent builder info.
getDefaultBuilderConfig :: IO BuilderConfig
getDefaultBuilderConfig
 = do   baseLibraryPath <- locateBaseLibrary
        return $ BuilderConfig
          { builderConfigBaseSrcDir     = baseLibraryPath
          , builderConfigBaseLibDir     = baseLibraryPath </> "build"
          , builderConfigLibFile        = \_static dynamic -> dynamic }


-- | Get the active builder.
--   If one is set explicitly in the state then use that, 
--   otherwise query the host system to determine the builder.
--   If that fails as well then 'error'.
getActiveBuilder :: State -> IO Builder 
getActiveBuilder state 
 = case stateBuilder state of
        Just builder          -> return builder
        Nothing         
         -> do  config   <- getDefaultBuilderConfig 
                mBuilder <- determineDefaultBuilder config
                case mBuilder of
                 Nothing      -> error "ddci-core.getActiveBuilder: unrecognised host platform"
                 Just builder -> return builder


