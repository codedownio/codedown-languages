{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -F -pgmF sandwich-discover -optF --module-prefix=Spec.Tests.Haskell. #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Spec.Tests.Haskell.LSP (tests) where

import Test.Sandwich
import TestLib.Contexts.GhcPackage
import TestLib.Types

#insert_test_imports


tests :: (
  HasBaseContext context, HasMaybeBubblewrap context, HasNixEnvironment context, HasGhcPackage context
  ) => SpecFree context IO ()
tests = $(getSpecFromFolder defaultGetSpecFromFolderOptions)
