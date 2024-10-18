{-# LANGUAGE TemplateHaskell #-}

module CodeDown.Languages.Deriving where

import Data.Aeson as A
import Data.Aeson.TypeScript.TH
import Language.Haskell.TH as TH


deriveJTH :: A.Options -> Name -> Name -> Q [Dec]
deriveJTH opts optsName name = do
  hasJsonDecl <- [d|instance HasJSONOptions $(conT name) where getJSONOptions _ = $(varE optsName)|]
  decls <- deriveJSONAndTypeScript opts name
  return (hasJsonDecl <> decls)
