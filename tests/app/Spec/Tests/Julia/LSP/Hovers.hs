{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Spec.Tests.Julia.LSP.Hovers (tests) where

import Control.Lens hiding (List)
import Data.String.Interpolate
import Language.LSP.Protocol.Lens hiding (hover, text)
import Language.LSP.Protocol.Types
import Spec.Tests.Julia.Common
import Test.Sandwich as Sandwich
import TestLib.LSP
import TestLib.Types


tests :: (LspContext context m, HasNixEnvironment context) => SpecFree context m ()
tests = itHasHoverSatisfying lsName "test.jl" juliaLanguageKind [__i|print("hi")|] (Position 0 2) $ \hover -> do
  let InL (MarkupContent MarkupKind_Markdown text) = hover ^. contents
  text `textShouldContain` "Write to `io` (or to the default output stream"
