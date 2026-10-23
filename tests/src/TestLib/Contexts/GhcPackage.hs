-- | Test discovery calls a no-argument 'tests', so the Haskell specs take the GHC version
-- they're running against from the context rather than as a parameter.
module TestLib.Contexts.GhcPackage (
  ghcPackage
  , HasGhcPackage
  , introduceGhcPackage
  , skippedOnGhc910
  ) where

import Control.Monad
import Control.Monad.IO.Class (MonadIO)
import Data.String.Interpolate
import Data.Text (Text)
import Test.Sandwich


ghcPackage :: Label "ghcPackage" Text
ghcPackage = Label

type HasGhcPackage context = HasLabel context "ghcPackage" Text

introduceGhcPackage :: (
  Monad m
  ) => Text -> SpecFree (LabelValue "ghcPackage" Text :> context) m () -> SpecFree context m ()
introduceGhcPackage pkg = introduce' nodeOpts [i|GHC package #{pkg}|] ghcPackage (return pkg) (const (return ()))

-- | Mark a spec pending on ghc910, where hlint output is broken.
-- TODO: re-enable the hlint tests on ghc910.
skippedOnGhc910 :: (
  HasGhcPackage context, MonadIO m
  ) => SpecFree context m () -> SpecFree context m ()
skippedOnGhc910 = before' nodeOpts "Skip on ghc910" $ do
  pkg <- getContext ghcPackage
  when (pkg == "ghc910") $ pendingWith "hlint output is broken on ghc910"

-- | Neither node does any work, so keep them out of the default view and out of the
-- run-root folder structure.
nodeOpts :: NodeOptions
nodeOpts = defaultNodeOptions {
  nodeOptionsRecordTime = False
  , nodeOptionsCreateFolder = False
  }
