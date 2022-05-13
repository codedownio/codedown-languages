{-# LANGUAGE TemplateHaskell #-}
-- {-# OPTIONS_GHC -F -pgmF sandwich-discover -optF --module-prefix=Spec. #-}

module Spec.Tests where

import Test.Sandwich

-- #insert_test_imports
import Spec.Tests.Python3Kernel as Python3Kernel


tests :: TopSpec
-- tests = $(getSpecFromFolder defaultGetSpecFromFolderOptions)
tests = Python3Kernel.tests
