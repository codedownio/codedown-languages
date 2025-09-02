
module Spec.Tests.Haskell.Info (stringInfo) where

import Data.Aeson as A
import qualified Data.Map as M
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Vector as V
import TestLib.JupyterTypes


-- | Expected output from typing ":info String"
stringInfo :: Text -> M.Map MimeType A.Value
stringInfo ghcPackage = M.fromList [
  (
    MimeType "text/html"
    , Array (V.fromList [
                String "<div class=\"code cm-s-hite\"><span class=\"keyword\">type</span><span class=\"space\"> </span><span class=\"variable-2\">String</span><span class=\"space\"> </span><span class=\"atom\">::</span><span class=\"space\"> </span><span class=\"atom\">*</span><span class=\"space\"><br /></span>\n"
                , String [i|<span class="keyword">type</span><span class="space"> </span><span class="variable-2">String</span><span class="space"> </span><span class="atom">=</span><span class="space"> </span><span class="atom">[</span><span class="variable-2">Char</span><span class="atom">]</span><span class="space"><br />  \t</span><span class="comment">-- Defined in \8216#{baseModuleName}\8217</span><span class="space"><br /></span></div>|]
                ])
  )
  , (
      MimeType "text/plain"
    , Array (V.fromList [
        String "type String :: *\n"
        , String "type String = [Char]\n"
        , String [i|  \t-- Defined in \8216#{baseModuleName}\8217|]
        ])
    )
  ]
  where
    baseModuleName :: Text
    baseModuleName = case ghcPackage of
      "ghc96" -> "GHC.Base"
      "ghc98" -> "GHC.Base"
      "ghc910" -> "GHC.Internal.Base"
      "ghc912" -> "GHC.Internal.Base"
      _ -> error [i|stringInfo: unexpected ghcPackage: #{ghcPackage}|]
