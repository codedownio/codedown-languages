{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -F -pgmF sandwich-discover -optF --module-prefix=Spec.Tests.Javascript. #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Spec.Tests.Javascript.LSP (tests) where

import Test.Sandwich
import TestLib.Types

#insert_test_imports


tests :: (
  HasBaseContext context, HasMaybeBubblewrap context, HasNixEnvironment context
  ) => SpecFree context IO ()
tests = $(getSpecFromFolder defaultGetSpecFromFolderOptions)
