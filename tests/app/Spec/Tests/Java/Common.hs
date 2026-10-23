module Spec.Tests.Java.Common (
  lsName
  , projectTimeout
  ) where

import Data.Text


lsName :: Text
lsName = "jdtls"

-- | JDT LS builds an Eclipse workspace and an "invisible project" for the folder before it can
-- answer anything beyond syntax errors, and on a cold workspace that takes a couple of
-- minutes. Every Java LSP test waits this long.
projectTimeout :: Double
projectTimeout = 300
