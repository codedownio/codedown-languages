{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module TestLib.Exporters where

import Data.Aeson as A
import Data.Aeson.QQ.Simple
import Data.Aeson.TH as A
import Data.String.Interpolate
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

sampleMdFile :: Text
sampleMdFile =
  [__i|\# Sample Markdown Document

       This is a sample markdown file to test exporter functionality.

       \#\# Features Demonstrated

       \#\#\# Text Formatting

       - **Bold text**
       - *Italic text*
       - `Inline code`
       - ~~Strikethrough text~~

       \#\#\# Code Blocks

       ```python
       def hello_world():
           print("Hello, World!")
           return 42
       ```

       \#\#\# Links and Images

       [Link to example](https://example.com)

       \#\#\# Tables

       | Column 1 | Column 2 | Column 3 |
       |----------|----------|----------|
       | Value A  | Value B  | Value C  |
       | Data 1   | Data 2   | Data 3   |

       \#\#\# Blockquotes

       > This is a blockquote.
       > It can span multiple lines.

       \#\#\# Math (if supported)

       Inline math: $x = y + z$

       Block math:
       $$\\sum_{i=1}^{n} x_i = x_1 + x_2 + \\cdots + x_n$$
       |]

sampleTypstFile :: Text
sampleTypstFile =
  [__i|\#set page(paper: "a4")
       \#set text(font: "Linux Libertine", size: 11pt)

       = Sample Typst Document

       This is a sample Typst file to test exporter functionality.

       == Features Demonstrated

       === Text Formatting

       - *Bold text*
       - _Italic text_
       - `Inline code`
       - \#strike[Strikethrough text]

       === Code Blocks

       ```python
       def hello_world():
           print("Hello, World!")
           return 42
       ```

       === Links and References

       \#link("https://example.com")[Link to example]

       === Tables

       \#table(
         columns: 3,
         [Column 1], [Column 2], [Column 3],
         [Value A], [Value B], [Value C],
         [Data 1], [Data 2], [Data 3]
       )

       === Lists

       1. Numbered list item
       2. Another numbered item
          - Nested bullet
          - Another nested bullet

       === Math

       Inline math: $x = y + z$

       Block math:
       $ sum_(i=1)^n i = (n(n+1))/2 $

       === Figures and Alignment

       \#align(center)[
         This text is centered
       ]

       === Custom Styling

       \#text(fill: blue)[This text is blue]

       \#rect(fill: gray.lighten(80%), inset: 8pt)[
         This is content in a gray box.
       ]
       |]
