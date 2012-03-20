
module DDCI.Core.Command.Sea
        (cmdSeaOut)
where
import DDC.Core.Load
import DDC.Core.Sea.Output.Profile
import DDC.Core.Sea.Output.Convert
import DDCI.Core.Fragment
import DDCI.Core.State
import DDCI.Core.IO
import DDC.Base.Pretty
import qualified Data.Set               as Set


-- | Parse, check, and fully evaluate an expression.
cmdSeaOut :: State -> Int -> String -> IO ()
cmdSeaOut state lineStart str
 = let  toks    = lexString lineStart str
   in   goLoad toks

 where  goLoad toks
         = case loadModule outputProfile "<interactive>"  toks of
                Left err -> putStrLn $ renderIndent $ ppr err
                Right mm -> goFragmentCheck mm

        goFragmentCheck mm
         = case fragmentCheckModule mm of
                Just err 
                 -> putStrLn 
                        $ renderIndent 
                        $ vcat  [ text "Fragment violation in SeaOut module."
                                , indent 2 (ppr err) ]

                Nothing  
                 -> goOutput mm

        goOutput mm
         = let  -- Include the Sea Prelude if we were asked for it.
                prelude  
                 | Set.member SeaPrelude (stateModes state)
                 = vcat  [ text "#include <Disciple.h>"
                         , text "#include <Primitive.h>" 
                         , line ]

                 | otherwise
                 = empty

           in   outDocLn state 
                 $    prelude <> convert mm
