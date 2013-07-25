
-- | Disciple Core Flow is a Domain Specific Language (DSL) for writing first
--   order data flow programs.
--   
module DDC.Core.Flow
        ( -- * Language profile
          profile

          -- * Names
        , Name          (..)
        , TyConFlow     (..)
        , PrimTyCon     (..)
        , PrimArith     (..)
        , PrimCast      (..)

          -- * Name Parsing
        , readName

          -- * Program Lexing
        , lexModuleString
        , lexExpString)

where
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Profile
