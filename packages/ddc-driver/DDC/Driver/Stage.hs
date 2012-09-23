-- | Compiler stages.
--     A compiler 'stage' is a set of standard transformations that we apply
--     to all modules their particular language.
module DDC.Driver.Stage
        ( Config (..)

          -- * Lite stages. 
        , stageLiteLoad
        , stageLiteOpt
        , stageLiteToSalt

          -- * Salt stages.
        , stageSaltOpt
        , stageSaltToC
        , stageSaltToLLVM

          -- * LLvm stages.
        , stageCompileLLVM)
where
import DDC.Driver.Source
import DDC.Build.Builder
import DDC.Build.Pipeline
import DDC.Build.Language
import DDC.Core.Transform.Namify
import DDC.Core.Check                           (AnTEC)
import DDC.Core.Simplifier                      (Simplifier)
import System.FilePath
import Data.Monoid
import Data.Maybe
import qualified DDC.Build.Language.Salt        as Salt
import qualified DDC.Build.Language.Lite        as Lite
import qualified DDC.Core.Simplifier            as S
import qualified DDC.Core.Lite                  as Lite
import qualified DDC.Core.Salt.Name             as Salt
import qualified DDC.Core.Salt.Runtime          as Salt


-- | Configuration for main compiler stages.
data Config
        = Config
        { -- | Dump intermediate code.
          configDump            :: Bool

          -- | Simplifiers to apply to intermediate code.
        , configSimplLite       :: Simplifier Int (AnTEC () Lite.Name) Lite.Name
        , configSimplSalt       :: Simplifier Int (AnTEC () Salt.Name) Salt.Name

          -- | The builder to use for the target architecture
        , configBuilder                 :: Builder

          -- | Suppress imports in Core modules
        , configSuppressCoreImports     :: Bool

          -- | Suppress the #import prelude in C modules.
        , configSuppressHashImports     :: Bool 

          -- | Override output file.
        , configOutputFile              :: Maybe FilePath

          -- | Override directory for build products.
        , configOutputDir               :: Maybe FilePath
        }


-------------------------------------------------------------------------------
-- | Load Lite.
stageLiteLoad
        :: Config -> Source
        -> [PipeCore (AnTEC () Lite.Name) Lite.Name]
        -> PipeText Lite.Name Lite.Error

stageLiteLoad config source pipesLite
 = PipeTextLoadCore fragmentLite
    ( PipeCoreOutput (dump config source "dump.lite-load.dcl")
    : pipesLite )


-------------------------------------------------------------------------------
-- | Optimise Lite.
stageLiteOpt 
        :: Config -> Source
        -> [PipeCore (AnTEC () Lite.Name) Lite.Name]
        -> PipeCore  (AnTEC () Lite.Name) Lite.Name

stageLiteOpt config source pipes
 = PipeCoreSimplify 
	fragmentLite
        (0 :: Int) 
        (configSimplLite config <> normalizeLite)
        [ PipeCoreOutput (dump config source "dump.lite-opt.dcl")
        , PipeCoreReCheck fragmentLite pipes ]

 where  -- The code fed to later stages must be normalized, 
        -- so ensure out simplifications are preserving this.
        normalizeLite
         = S.anormalize
                (makeNamifier Lite.freshT)      
                (makeNamifier Lite.freshX)

-------------------------------------------------------------------------------
-- | Optimise Salt.
stageSaltOpt
        :: Config -> Source
        -> [PipeCore (AnTEC () Salt.Name) Salt.Name]
        -> PipeCore  (AnTEC () Salt.Name) Salt.Name

stageSaltOpt config source pipes
 = PipeCoreSimplify 
	fragmentSalt
        (0 :: Int) 
        (configSimplSalt config <> normalizeSalt)
        ( PipeCoreOutput (dump config source "dump.salt-opt.dcl")
        : pipes)

 where  -- The code fed to later stages must be normalized,
        -- so ensure out simplifications are preserving this.
        normalizeSalt
         = S.anormalize
                (makeNamifier Salt.freshT)      
                (makeNamifier Salt.freshX)


-------------------------------------------------------------------------------
-- | Convert Lite to Salt.
--   
--   Result is a-normalised.
--
stageLiteToSalt 
        :: Config -> Source
        -> [PipeCore (AnTEC () Salt.Name) Salt.Name] 
        -> PipeCore  (AnTEC () Lite.Name) Lite.Name

stageLiteToSalt config source pipesSalt
 = PipeCoreSimplify         fragmentLite 0 normalizeLite
     [ PipeCoreOutput           (dump config source "dump.lite-normalized.dcl")
     , PipeCoreReCheck          fragmentLite
       [ PipeCoreAsLite 
         [ PipeLiteToSalt       (buildSpec $ configBuilder config) runConfig
           [ PipeCoreOutput     (dump config source "dump.lite-to-salt.dce")
           , PipeCoreSimplify fragmentSalt 0 normalizeSalt
             [ PipeCoreCheck    fragmentSalt
               ( PipeCoreOutput (dump config source "dump.salt-normalized.dce")
               : pipesSalt)]]]]]

 where  -- Set the default runtime system parameters.
        -- TODO: We should be able to set this from the command line.
        runConfig
         = Salt.Config
         { Salt.configHeapSize   = 1000 }

        normalizeLite
         = S.anormalize
                (makeNamifier Lite.freshT)      
                (makeNamifier Lite.freshX)

        normalizeSalt
         = S.anormalize
                (makeNamifier Salt.freshT)      
                (makeNamifier Salt.freshX)


-------------------------------------------------------------------------------
-- | Convert Salt to C code.
stageSaltToC
        :: Config -> Source
        -> Sink
        -> PipeCore (AnTEC () Salt.Name) Salt.Name

stageSaltToC config source sink
 = PipeCoreSimplify fragmentSalt 0
        (configSimplSalt config 
                <> S.anormalize (makeNamifier Salt.freshT) 
                                (makeNamifier Salt.freshX))
   [ PipeCoreOutput       (dump config source "dump.salt-simplified.dce")
   , PipeCoreCheck        fragmentSalt
     [ PipeCoreAsSalt
       [ PipeSaltTransfer
         [ PipeSaltOutput (dump config source "dump.salt-transfer.dce")
         , PipeSaltPrint  
                (not $ configSuppressHashImports config)
                sink]]]]


-------------------------------------------------------------------------------
-- | Convert Salt to LLVM.
stageSaltToLLVM
        :: Config -> Source
        -> [PipeLlvm]
        -> PipeCore (AnTEC () Salt.Name) Salt.Name

stageSaltToLLVM config source pipesLLVM
 = PipeCoreSimplify fragmentSalt 0
        (configSimplSalt config
                <> S.anormalize (makeNamifier Salt.freshT)
                                (makeNamifier Salt.freshX))
   [ PipeCoreOutput         (dump config source "dump.salt-simplified.dce")
   , PipeCoreCheck          fragmentSalt
     [ PipeCoreAsSalt
       [ PipeSaltTransfer
         [ PipeSaltOutput   (dump config source "dump.salt-transfer.dce")
         , PipeSaltToLlvm   (buildSpec $ configBuilder config) 
           ( PipeLlvmPrint  (dump config source "dump.salt-to-llvm.ll")
           : pipesLLVM) ]]]]


-------------------------------------------------------------------------------
-- | Compile LLVM code.
stageCompileLLVM 
        :: Config -> Source
        -> FilePath             -- ^ Path of original source file.
                                --   Build products are placed into the same dir.
        -> Bool                 -- ^ Should we link this into an executable
        -> PipeLlvm

stageCompileLLVM config _source filePath shouldLinkExe
 = let  -- Decide where to place the build products.
        outputDir      = fromMaybe (takeDirectory filePath) (configOutputDir config)
        outputDirBase  = dropExtension (replaceDirectory filePath outputDir)
        llPath         = outputDirBase ++ ".ddc.ll"
        sPath          = outputDirBase ++ ".ddc.s"
        oPath          = outputDirBase ++ ".o"
        exePathDefault = outputDirBase
        exePath        = fromMaybe exePathDefault (configOutputFile config)
   in   -- Make the pipeline for the final compilation.
        PipeLlvmCompile
          { pipeBuilder           = configBuilder config
          , pipeFileLlvm          = llPath
          , pipeFileAsm           = sPath
          , pipeFileObject        = oPath
          , pipeFileExe           = if shouldLinkExe 
                                        then Just exePath 
                                        else Nothing }


------------------------------------------------------------------------------
-- | If the Dump mode is set 
--    then produce a SinkFile to write a module to a file, 
--    otherwise produce SinkDiscard to drop it on the floor.
dump :: Config -> Source -> String -> Sink
dump config source dumpFile 
        | configDump config
        = let   outputDir
                 | SourceFile filePath  <- source
                 = fromMaybe (takeDirectory filePath) 
                             (configOutputDir config)

                 | otherwise
                 = fromMaybe "."
                             (configOutputDir config)

          in    SinkFile $ outputDir </> dumpFile

        | otherwise
        = SinkDiscard
