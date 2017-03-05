{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Lish types
module Lish.Types
  ( SExp(..)
  , show
  , repr
  , Env
  , CmdStream
  , Command
  , ReduceUnawareCommand
  -- types
  , LishType(..)
  , Context
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Text       as Text
import           GHC.IO.Handle   (Handle)
import           GHC.Show        (Show (..))
import           Protolude       hiding (show)

data SExp = Atom Text
          | Num Integer
          | Bool Bool
          | Str Text
          | List [SExp]
          | Lambda [SExp]
          | Void
          -- only exists during evaluation
          | Fn { params  :: [Text]
               , body    :: SExp
               , closure :: Env
               , types :: ([LishType],LishType)
               }
          | Stream CmdStream
          | WaitingStream CmdStream
          deriving (Eq,Show)

data LishType = LAtom
              | LNum
              | LBool
              | LStr
              | LList LishType
              | LFn [LishType] LishType
              | LVoid
              deriving (Eq,Show)

type Context = Map.Map Text LishType

repr :: SExp -> Text
repr (Atom s)          = s
repr (Num n)           = toS $ show n
repr (Bool b)          = if b then "true" else "false"
repr (Str s)           = "\"" <> toS s <> "\""
repr (List sexprs)     = "[" <> (Text.intercalate " " (map repr sexprs)) <> "]"
repr (Lambda sexprs)   = "(" <> (Text.intercalate " " (map repr sexprs)) <> ")"
repr Void              = "ε"
repr (Fn p _ _)        = "(λ" <> (Text.intercalate "." p) <> ". ... )"
repr (Stream _)        = "<stream>"
repr (WaitingStream _) = "<w-stream>"

type CmdStream = Maybe Handle
type Env = Map.Map Text SExp
type ReduceUnawareCommand = [SExp] -> StateT Env IO SExp
type Command = (SExp -> StateT Env IO SExp) -> ReduceUnawareCommand
