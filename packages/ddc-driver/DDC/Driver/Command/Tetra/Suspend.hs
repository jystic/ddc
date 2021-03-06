
module DDC.Driver.Command.Tetra.Suspend
        (cmdTetraSuspend)
where
import DDC.Driver.Stage
import DDC.Driver.Config
import DDC.Driver.Dump
import DDC.Interface.Source
import DDC.Build.Pipeline
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import System.FilePath
import qualified DDC.Core.Check                 as C
import qualified DDC.Base.Pretty                as P
import qualified DDC.Build.Language.Tetra       as Tetra


-- | Manage higher order functions in a Tetra module.
cmdTetraSuspend
        :: Config               -- ^ Driver config.
        -> Source               -- ^ Source of the code.
        -> String               -- ^ Program module text.
        -> ExceptT String IO ()

cmdTetraSuspend config source sourceText

 -- Curring only works for Disciple Core Tetra files.
 | SourceFile filePath  <- source
 , ext  <- takeExtension filePath 
 , ext /= ".dct"
 = throwE $ "The Suspend transform only works for Core Tetra (.dct) modules."

 | otherwise
 = let  pmode   = prettyModeOfConfig $ configPretty config

        pipeCurry
         = pipeText     (nameOfSource source)
                        (lineStartOfSource source) sourceText
         $ PipeTextLoadCore Tetra.fragment C.Recon SinkDiscard
         [ PipeCoreAsTetra
         [ PipeTetraSuspend
         [ PipeCoreOutput   pmode (dump config source "dump.tetra-curry.dct")
         , PipeCoreCheck    Tetra.fragment C.Recon SinkDiscard
           [ PipeCoreOutput pmode SinkStdout ]]]]

   in do
        errs    <- liftIO pipeCurry
        case errs of
         []     -> return ()
         es     -> throwE $ P.renderIndent $ P.vcat $ map P.ppr es

