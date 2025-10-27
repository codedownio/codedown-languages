{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.Exporters (tests) where

import Data.String.Interpolate
import Data.Text (Text)
import Test.Sandwich as Sandwich
import TestLib.Exporters
import TestLib.JupyterRunnerContext (introduceJustBubblewrap)
import TestLib.NixEnvironmentContext
import TestLib.Types


tests :: LanguageSpec
tests = describe "Exporters" $ introduceJustBubblewrap $ do
  -- testTexliveScheme "scheme-minimal"
  -- testTexliveScheme "scheme-basic"
  -- testTexliveScheme "scheme-small"
  -- testTexliveScheme "scheme-medium"
  -- testTexliveScheme "scheme-bookpub"
  -- testTexliveScheme "scheme-tetex"

  testTexliveScheme "scheme-full"

  introduceNixEnvironment [] [pandocConfig] [i|Exporters (pandoc)|] $ do
    it "codedown-exporter-slidy" $ testExportMd "codedown-exporter-slidy" "html"
    it "codedown-exporter-beamer" $ testExportMd "codedown-exporter-beamer" "html"
    it "codedown-exporter-pdf" $ testExportMd "codedown-exporter-pdf" "pdf"

  introduceNixEnvironment [] [typstConfig] [i|Exporters (typst)|] $ do
    it "codedown-exporter-typst" $ testExportTypst "codedown-exporter-typst" "pdf"


testTexliveScheme :: Text -> LanguageSpec
testTexliveScheme scheme = do
  introduceNixEnvironment [] [nbconvertConfig scheme] [i|Exporters (nbconvert, #{scheme})|] $ do
    it "codedown-exporter-asciidoc" $ testExportIpynb "codedown-exporter-asciidoc" "asciidoc"
    it "codedown-exporter-latex" $ testExportIpynb "codedown-exporter-latex" "tex"
    it "codedown-exporter-pdf" $ testExportIpynb "codedown-exporter-pdf" "pdf"
    it "codedown-exporter-html" $ testExportIpynb "codedown-exporter-html" "html"
    it "codedown-exporter-rst" $ testExportIpynb "codedown-exporter-rst" "rst"
    it "codedown-exporter-markdown" $ testExportIpynb "codedown-exporter-markdown" "md"
    -- it "codedown-exporter-slides" $ testExport "codedown-exporter-slides" "slides.html"


nbconvertConfig :: Text -> Text
nbconvertConfig scheme = [__i|exporters.nbconvert.enable = true;
                              exporters.nbconvert.texliveScheme = "#{scheme}";|]

pandocConfig :: Text
pandocConfig = [__i|exporters.pandoc.enable = true;|]

typstConfig :: Text
typstConfig = [__i|exporters.typst.enable = true;|]
