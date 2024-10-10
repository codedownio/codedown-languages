
module TestLib.TH (
  getFileListRelativeToRoot
  ) where

import Language.Haskell.TH
import System.FilePath
import UnliftIO.Directory
import UnliftIO.Exception


findGitRoot :: FilePath -> IO (Maybe FilePath)
findGitRoot dir = do
  let gitDir = dir </> ".git"
  doesDirectoryExist gitDir >>= \case
    True -> return (Just dir)
    False -> do
      let parent = takeDirectory dir
      if parent == dir  -- We've reached the root directory
        then return Nothing
        else findGitRoot parent

getFileListRelativeToRoot :: FilePath -> Q Exp
getFileListRelativeToRoot subDir = do
  gitRoot <- runIO (getCurrentDirectory >>= findGitRoot >>= \case
                       Nothing -> throwIO $ userError "Couldn't find git root"
                       Just x -> pure x
                   )

  let path = gitRoot </> subDir

  contents <- runIO $ getDirectoryContents path
  let files = Prelude.filter (`notElem` [".", ".."]) contents
  listE $ fmap (litE . stringL) files
