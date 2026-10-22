{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -F -pgmF sandwich-discover -optF --module-prefix=Spec.Tests.Javascript. #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Spec.Tests.Javascript.Lsp (tests) where

import Test.Sandwich
import TestLib.Types

#insert_test_imports


tests :: (
  HasBaseContext context, HasMaybeBubblewrap context, HasNixEnvironment context
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
