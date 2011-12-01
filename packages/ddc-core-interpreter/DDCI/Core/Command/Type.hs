{-# OPTIONS -fno-warn-missing-signatures #-}
module DDCI.Core.Command.Type
        ( cmdShowType
        , ShowTypeMode(..))
where
import DDCI.Core.Prim.Env
import DDCI.Core.Prim.Name
import DDC.Core.Check
import DDC.Core.Pretty
import DDC.Core.Parser.Lexer
import DDC.Core.Parser
import qualified DDC.Core.Transform     as T
import qualified DDC.Base.Parser        as BP


-- | What components of the checked type to display.
data ShowTypeMode
        = ShowTypeAll
        | ShowTypeValue
        | ShowTypeEffect
        | ShowTypeClosure
        deriving (Eq, Show)


-- | Show the type of an expression.
cmdShowType :: ShowTypeMode -> String -> IO ()
cmdShowType mode ss
        = goParse mode (lexExp Name ss)

goParse mode toks                
 = case BP.runTokenParser show "<interactive>" pExp toks of 
    Left err -> putStrLn $ "parse error " ++ show err
    Right x  
     -> let x'  = T.spread primEnv x
        in  goCheck mode x' (checkExp primEnv x')

        
goCheck _ _ (Left err)
        = putStrLn $ show $ ppr err

goCheck mode x (Right (t, eff, clo))
 = case mode of
        ShowTypeAll
         -> putStrLn $ show 
                $ vcat 
                [ ppr x
                , nest 4 $ text ":: " <> ppr t
                , nest 4 $ text ":!: " <> ppr eff
                , nest 4 $ text ":$: " <> ppr clo]
        
        ShowTypeValue
         -> putStrLn $ pretty 100 (ppr x <> text " :: " <> ppr t)
        
        ShowTypeEffect
         -> putStrLn $ pretty 100 (ppr x <> text " :! " <> ppr eff)

        ShowTypeClosure
         -> putStrLn $ pretty 100 (ppr x <> text " :$ " <> ppr clo)
