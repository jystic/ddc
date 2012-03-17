
-- | Reference lexer for core langauge parser. Slow but Simple.
module DDC.Core.Parser.Lexer
        ( -- * Constructors
          isConName, isConStart, isConBody
        , readTwConBuiltin
        , readTcConBuiltin
        , readWbConBuiltin
        , readCon
        
          -- * Variables
        , isVarName, isVarStart, isVarBody
        , readVar

          -- * Lexer
        , lexExp)
where
import DDC.Base.Lexer
import DDC.Core.Exp
import DDC.Core.Parser.Tokens
import Data.Char


-- WbCon names ----------------------------------------------------------------
-- | Read a `WbCon`.
readWbConBuiltin :: String -> Maybe WbCon
readWbConBuiltin ss
 = case ss of
        "pure"          -> Just WbConPure
        "empty"         -> Just WbConEmpty
        "use"           -> Just WbConUse
        "read"          -> Just WbConRead
        "alloc"         -> Just WbConAlloc
        _               -> Nothing


-- | Textual keywords in the core language.
keywords :: [(String, Tok n)]
keywords
 =      [ ("module",     KA KModule)
        , ("imports",    KA KImports)
        , ("exports",    KA KExports)
        , ("in",         KA KIn)
        , ("of",         KA KOf) 
        , ("letrec",     KA KLetRec)
        , ("letregion",  KA KLetRegion)
        , ("withregion", KA KWithRegion)
        , ("let",        KA KLet)
        , ("lazy",       KA KLazy)
        , ("case",       KA KCase)
        , ("purify",     KA KPurify)
        , ("forget",     KA KForget)
        , ("weakeff",    KA KWeakEff)
        , ("weakclo",    KA KWeakClo)
        , ("with",       KA KWith)
        , ("where",      KA KWhere) ]


-------------------------------------------------------------------------------
-- | Lex a string into tokens.
--
lexExp :: Int -> String -> [Token (Tok String)]
lexExp lineStart str
 = lexWord lineStart 1 str
 where 

  lexWord :: Int -> Int -> String -> [Token (Tok String)]
  lexWord line column w
   = let  tok t = Token t (SourcePos Nothing line column)
          tokA  = tok . KA
          tokN  = tok . KN

          lexMore n rest
           = lexWord line (column + n) rest

     in case w of
        []               -> []        

        ' '  : w'        -> lexMore 1 w'
        '\t' : w'        -> lexMore 8 w'
        '\n' : w'        -> lexWord (line + 1) 1 w'


        -- The unit data constructor
        '(' : ')' : w'   -> tokN (KCon "()")     : lexMore 2 w'

        -- Compound Parens
        '['  : ':' : w'  -> tokA KSquareColonBra : lexMore 2 w'
        ':'  : ']' : w'  -> tokA KSquareColonKet : lexMore 2 w'
        '<'  : ':' : w'  -> tokA KAngleColonBra  : lexMore 2 w'
        ':'  : '>' : w'  -> tokA KAngleColonKet  : lexMore 2 w'

        -- Function Constructors
        '~'  : '>'  : w' -> tokA KArrowTilde     : lexMore 2 w'
        '-'  : '>'  : w' -> tokA KArrowDash      : lexMore 2 w'
        '='  : '>'  : w' -> tokA KArrowEquals    : lexMore 2 w'

        -- Compound symbols
        ':'  : ':'  : w' -> tokA KColonColon     : lexMore 2 w'
        '/'  : '\\' : w' -> tokA KBigLambda      : lexMore 2 w'

        -- Debruijn indices
        '^'  : cs
         |  (ds, rest)   <- span isDigit cs
         ,  length ds >= 1
         -> tokA (KIndex (read ds))              : lexMore (1 + length ds) rest         

        -- Parens
        '('  : w'       -> tokA KRoundBra        : lexMore 1 w'
        ')'  : w'       -> tokA KRoundKet        : lexMore 1 w'
        '['  : w'       -> tokA KSquareBra       : lexMore 1 w'
        ']'  : w'       -> tokA KSquareKet       : lexMore 1 w'
        '{'  : w'       -> tokA KBraceBra        : lexMore 1 w'
        '}'  : w'       -> tokA KBraceKet        : lexMore 1 w'
        '<'  : w'       -> tokA KAngleBra        : lexMore 1 w'
        '>'  : w'       -> tokA KAngleKet        : lexMore 1 w'            

        -- Punctuation
        '.'  : w'       -> tokA KDot             : lexMore 1 w'
        '|'  : w'       -> tokA KBar             : lexMore 1 w'
        '^'  : w'       -> tokA KHat             : lexMore 1 w'
        '+'  : w'       -> tokA KPlus            : lexMore 1 w'
        ':'  : w'       -> tokA KColon           : lexMore 1 w'
        ','  : w'       -> tokA KComma           : lexMore 1 w'
        '\\' : w'       -> tokA KBackSlash       : lexMore 1 w'
        ';'  : w'       -> tokA KSemiColon       : lexMore 1 w'
        '_'  : w'       -> tokA KUnderscore      : lexMore 1 w'
        '='  : w'       -> tokA KEquals          : lexMore 1 w'
        '&'  : w'       -> tokA KAmpersand       : lexMore 1 w'
        '-'  : w'       -> tokA KDash            : lexMore 1 w'
        
        -- Bottoms
        '!' : '0' : w'  -> tokA KBotEffect       : lexMore 2 w'
        '$' : '0' : w'  -> tokA KBotClosure      : lexMore 2 w'

        -- Sort Constructors
        '*' : '*' : w'  -> tokA KSortComp        : lexMore 2 w'
        '@' : '@' : w'  -> tokA KSortProp        : lexMore 2 w'        

        -- Kind Constructors
        '*' : w'        -> tokA KKindValue       : lexMore 1 w'
        '%' : w'        -> tokA KKindRegion      : lexMore 1 w'
        '!' : w'        -> tokA KKindEffect      : lexMore 1 w'
        '$' : w'        -> tokA KKindClosure     : lexMore 1 w'
        '@' : w'        -> tokA KKindWitness     : lexMore 1 w'
        
        -- Literal values
        c : cs
         | isDigit c
         , (body, rest)         <- span isDigit cs
         -> tokN (KLit (c:body))                 : lexMore (length (c:body)) rest
        
        -- Named Constructors
        c : cs
         | isConStart c
         , (body,  rest)        <- span isConBody cs
         , (body', rest')       <- case rest of
                                        '#' : rest'     -> (body ++ "#", rest')
                                        _               -> (body, rest)
         -> let readNamedCon s
                 | Just twcon   <- readTwConBuiltin s
                 = tokA (KTwConBuiltin twcon)    : lexMore (length s) rest'
                 
                 | Just tccon   <- readTcConBuiltin s
                 = tokA (KTcConBuiltin tccon)    : lexMore (length s) rest'
                 
                 | Just con     <- readCon s
                 = tokN (KCon con)               : lexMore (length s) rest'
               
                 | otherwise    
                 = [tok (KJunk c)]
                 
            in  readNamedCon (c : body')

        -- Keywords, Named Variables and Witness constructors
        c : cs
         | isVarStart c
         , (body,  rest)        <- span isVarBody cs
         , (body', rest')       <- case rest of
                                        '#' : rest'     -> (body ++ "#", rest')
                                        _               -> (body, rest)
         -> let readNamedVar s
                 | Just t <- lookup s keywords
                 = tok t                   : lexMore (length s) rest'

                 | Just wc      <- readWbConBuiltin s
                 = tokA (KWbConBuiltin wc) : lexMore (length s) rest'
         
                 | Just v       <- readVar s
                 = tokN (KVar v)           : lexMore (length s) rest'

                 | otherwise
                 = [tok (KJunk c)]

            in  readNamedVar (c : body')

        -- Error
        c : _   -> [tok $ KJunk c]
        

-- TyCon names ----------------------------------------------------------------
-- | String is a constructor name.
isConName :: String -> Bool
isConName str
 = case str of
     []          -> False
     (c:cs)      
        | isConStart c 
        , and (map isConBody cs)
        -> True
        
        | _ : _         <- cs
        , isConStart c
        , and (map isConBody (init cs))
        , last cs == '#'
        -> True

        | otherwise
        -> False

-- | Character can start a constructor name.
isConStart :: Char -> Bool
isConStart = isUpper


-- | Charater can be part of a constructor body.
isConBody  :: Char -> Bool
isConBody c           = isUpper c || isLower c || isDigit c || c == '_'
        

-- | Read a named `TwCon`. 
readTwConBuiltin :: String -> Maybe TwCon
readTwConBuiltin ss
 = case ss of
        "Global"        -> Just TwConGlobal
        "DeepGlobal"    -> Just TwConDeepGlobal
        "Const"         -> Just TwConConst
        "DeepConst"     -> Just TwConDeepConst
        "Mutable"       -> Just TwConMutable
        "DeepMutable"   -> Just TwConDeepMutable
        "Lazy"          -> Just TwConLazy
        "HeadLazy"      -> Just TwConHeadLazy
        "Manifest"      -> Just TwConManifest
        "Pure"          -> Just TwConPure
        "Empty"         -> Just TwConEmpty
        _               -> Nothing


-- | Read a builtin `TcCon` with a non-symbolic name, 
--   ie not '->'.
readTcConBuiltin :: String -> Maybe TcCon
readTcConBuiltin ss
 = case ss of
        "Read"          -> Just TcConRead
        "HeadRead"      -> Just TcConHeadRead
        "DeepRead"      -> Just TcConDeepRead
        "Write"         -> Just TcConWrite
        "DeepWrite"     -> Just TcConDeepWrite
        "Alloc"         -> Just TcConAlloc
        "DeepAlloc"     -> Just TcConDeepAlloc
        "Use"           -> Just TcConUse
        "DeepUse"       -> Just TcConDeepUse
        _               -> Nothing


-- | Read a named, user defined `TcCon`.
readCon :: String -> Maybe String
readCon ss
        | isConName ss  = Just ss
        | otherwise     = Nothing


-- TyVar names ----------------------------------------------------------------
-- | String is a variable name
isVarName :: String -> Bool
isVarName str
 = case str of
     []          -> False
     (c:cs)      
        | isVarStart c 
        , and (map isVarBody cs)
        -> True
        
        | _ : _         <- cs
        , isVarStart c
        , and (map isVarBody (init cs))
        , last cs == '#'
        -> True

        | otherwise
        -> False


-- | Charater can start a variable name.
isVarStart :: Char -> Bool
isVarStart = isLower
        

-- | Character can be part of a variable body.
isVarBody  :: Char -> Bool
isVarBody c
        = isUpper c || isLower c || isDigit c || c == '_' || c == '\''


-- | Read a named, user defined variable.
readVar :: String -> Maybe String
readVar ss
        | isVarName ss  = Just ss
        | otherwise     = Nothing

