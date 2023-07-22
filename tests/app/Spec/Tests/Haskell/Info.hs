
module Spec.Tests.Haskell.Info (stringInfo) where

import Data.Aeson as A
import qualified Data.Map as M
import qualified Data.Vector as V
import TestLib.JupyterTypes


-- | Expected output from typing ":info String"
stringInfo :: M.Map MimeType A.Value
stringInfo = M.fromList [
  (
    MimeType "text/html"
    , Array (V.fromList [
                String "<div class=\"code cm-s-hite\"><span class=\"keyword\">type</span><span class=\"space\"> </span><span class=\"variable-2\">String</span><span class=\"space\"> </span><span class=\"atom\">::</span><span class=\"space\"> </span><span class=\"atom\">*</span><span class=\"space\"><br /></span>\n"
                , String "<span class=\"keyword\">type</span><span class=\"space\"> </span><span class=\"variable-2\">String</span><span class=\"space\"> </span><span class=\"atom\">=</span><span class=\"space\"> </span><span class=\"atom\">[</span><span class=\"variable-2\">Char</span><span class=\"atom\">]</span><span class=\"space\"><br />  \t</span><span class=\"comment\">-- Defined in \8216GHC.Base\8217</span><span class=\"space\"><br /></span></div>"
                ])
  )
  , (
      MimeType "text/plain"
    , Array (V.fromList [
        String "type String :: *\n"
        , String "type String = [Char]\n"
        , String "  \t-- Defined in \8216GHC.Base\8217"
        ])
    )
  ]
