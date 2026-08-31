
module TestLib.TH (
  getFileListRelativeToRoot
  ) where

import Language.Haskell.TH
import System.FilePath
import UnliftIO.Directory
import UnliftIO.Exception


-- | Walk up looking for the directory itself, rather than for a .git. Under Nix the source is a
-- store path with no repository in it.
findDirAbove :: FilePath -> FilePath -> IO (Maybe FilePath)
findDirAbove subDir dir = doesDirectoryExist (dir </> subDir) >>= \case
  True -> return (Just dir)
  False -> case takeDirectory dir of
    parent | parent == dir -> return Nothing
           | otherwise -> findDirAbove subDir parent

getFileListRelativeToRoot :: FilePath -> Q Exp
getFileListRelativeToRoot subDir = do
  root <- runIO (getCurrentDirectory >>= findDirAbove subDir >>= \case
                    Nothing -> throwIO $ userError ("Couldn't find " <> subDir <> " in any parent")
                    Just x -> pure x
                )

  contents <- runIO $ getDirectoryContents (root </> subDir)
  let files = Prelude.filter (`notElem` [".", ".."]) contents
  listE $ fmap (litE . stringL) files
