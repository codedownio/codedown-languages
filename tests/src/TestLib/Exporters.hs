{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module TestLib.Exporters where

import Data.Aeson as A
import Data.Aeson.QQ.Simple
import Data.Aeson.TH as A
import Data.Text as T
import TestLib.Aeson

data ExporterInfo = ExporterInfo {
  exporterInfoName :: Text
  , exporterInfoDisplayName :: Text
  , exporterInfoIcon :: Maybe Text
  , exporterInfoArgs :: [Text]
  , exporterInfoExtension :: Text
} deriving (Show, Eq)
deriveJSON toSnake2 ''ExporterInfo

sampleJupyterNotebook :: Value
sampleJupyterNotebook = [aesonQQ|{
  "cells": [
    {
      "cell_type": "code",
      "execution_count": null,
      "metadata": {},
      "outputs": [],
      "source": ["print('Hello from exporter test')"]
    }
  ],
  "metadata": {
    "kernelspec": {
      "display_name": "Python 3",
      "language": "python",
      "name": "python3"
    }
  },
  "nbformat": 4,
  "nbformat_minor": 4
}|]
