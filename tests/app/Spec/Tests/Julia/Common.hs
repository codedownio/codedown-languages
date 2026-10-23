module Spec.Tests.Julia.Common (
  lsName
  , juliaLanguageKind
  ) where

import Data.Text (Text)
import Language.LSP.Protocol.Types


lsName :: Text
lsName = "LanguageServer"

juliaLanguageKind :: LanguageKind
juliaLanguageKind = LanguageKind_Custom "julia"
