{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -F -pgmF sandwich-discover -optF --module-prefix=Spec.Tests.Haskell. #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Spec.Tests.Haskell.Lsp (tests) where

import Spec.Tests.Haskell.GhcPackage
import Test.Sandwich
import TestLib.Types

#insert_test_imports


tests :: (
  HasBaseContext context, HasMaybeBubblewrap context, HasNixEnvironment context, HasGhcPackage context
  ) => SpecFree context IO ()
tests = $(getSpecFromFolder (defaultGetSpecFromFolderOptions {
             getSpecCombiner = 'describeLsp
             , getSpecIndividualSpecHooks = 'ignoringFileName
             }))

-- | The folder is named Lsp, but the node in the test tree should read "LSP".
describeLsp :: FilePath -> SpecFree context IO () -> SpecFree context IO ()
describeLsp _ = describe "LSP"

ignoringFileName :: FilePath -> SpecFree context IO () -> SpecFree context IO ()
ignoringFileName _ = id
