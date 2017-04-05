{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Lish types
module Lish.Types
  ( SExp
  , Expr
  , ExprF(..)
  , show
  , repr
  , pprint
  , Env
  , CmdStream
  , Command
  , ReduceUnawareCommand
  -- types
  , LishType(..)
  , Context
  )
where

import           Data.Fix
import           Data.Map.Strict (Map)
import qualified Data.Text       as Text
import           GHC.IO.Handle   (Handle)
import           GHC.Show        (Show (..))
import           Protolude       hiding (show)

data ExprF a = Atom Text
             | Num Integer
             | Bool Bool
             | Str Text
             | List [a]
             | Lambda [a]
             | Void
              -- only exists during evaluation
             | Fn { params  :: [Text]
                  , body    :: a
                  , closure :: Env
                  , types   :: ([LishType],LishType)
                  }
             | Command { _cmdName :: a
                       , _cmdArgs :: [a]}
             | Stream CmdStream
             | WaitingStream CmdStream
             deriving (Eq,Show,Functor)

type Expr = Fix ExprF
type SExp = ExprF Expr

data LishType = LCommand
              | LNum
              | LBool
              | LStr
              | LList LishType
              | LFn [LishType] LishType
              | LVoid
              deriving (Eq,Show)

type Context = Map Text LishType

repr :: ExprF Text -> Text
repr (Atom s)          = s
repr (Num n)           = toS $ show n
repr (Bool b)          = if b then "true" else "false"
repr (Str s)           = "\"" <> toS s <> "\""
repr (List sexprs)     = "[" <> (Text.intercalate " " sexprs) <> "]"
repr (Lambda sexprs)   = "(" <> (Text.intercalate " " sexprs) <> ")"
repr Void              = "ε"
repr (Command n args)  = "($ " <> n <> (Text.intercalate " " args) <> ")"
repr (Fn p _ _ _)      = "(λ" <> (Text.intercalate "." p) <> ". ... )"
repr (Stream _)        = "<stream>"
repr (WaitingStream _) = "<w-stream>"

pprint :: Expr -> Text
pprint = cata repr

type CmdStream = Maybe Handle
type Env = Map Text SExp
type ReduceUnawareCommand = [SExp] -> StateT Env IO SExp
type Command = (SExp -> StateT Env IO SExp) -> ReduceUnawareCommand
