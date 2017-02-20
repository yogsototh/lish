{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Lish types
module Lish.Types
  ( SExp(..)
  , show
  , Env
  , CmdStream
  , Command
  )
where

import qualified Data.Text     as Text
import           GHC.IO.Handle (Handle)
import           GHC.Show      (Show (..))
import           Protolude     hiding (show)

data SExp = Lambda [SExp]
          | Atom Text
          | List [SExp]
          | Str Text
          | Void
          -- only exists during evaluation
          | Stream CmdStream
          | WaitingStream CmdStream

instance Show SExp where
  show = toS . repr

repr :: SExp -> Text
repr (Atom s)        = s
repr (Str s)         = "\"" <> toS s <> "\""
repr (Lambda sexprs) = "(λ." <> (Text.intercalate " " (map repr sexprs)) <> ")"
repr (List sexprs)   = "[" <> (Text.intercalate " " (map repr sexprs)) <> "]"
repr Void            = "ε"
repr (Stream _)        = "<stream>"
repr (WaitingStream _) = "<w-stream>"

type CmdStream = Maybe Handle
type Env = [(Text,SExp)]
type Command = [SExp] -> StateT Env IO SExp
