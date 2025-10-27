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
    it "codedown-exporter-pandoc-slidy" $ testExportMd "codedown-exporter-pandoc-slidy" "html"
    it "codedown-exporter-pandoc-beamer" $ testExportMd "codedown-exporter-pandoc-beamer" "html"
    it "codedown-exporter-pandoc-pdf" $ testExportMd "codedown-exporter-pandoc-pdf" "pdf"

  introduceNixEnvironment [] [typstConfig] [i|Exporters (typst)|] $ do
    it "codedown-exporter-typst" $ testExportTypst "codedown-exporter-typst" "pdf"


testTexliveScheme :: Text -> LanguageSpec
testTexliveScheme scheme = do
  introduceNixEnvironment [] [nbconvertConfig scheme] [i|Exporters (nbconvert, #{scheme})|] $ do
    it "codedown-exporter-nbconvert-asciidoc" $ testExportIpynb "codedown-exporter-nbconvert-asciidoc" "asciidoc"
    it "codedown-exporter-nbconvert-latex" $ testExportIpynb "codedown-exporter-nbconvert-latex" "tex"
    it "codedown-exporter-nbconvert-pdf" $ testExportIpynb "codedown-exporter-nbconvert-pdf" "pdf"
    it "codedown-exporter-nbconvert-html" $ testExportIpynb "codedown-exporter-nbconvert-html" "html"
    it "codedown-exporter-nbconvert-rst" $ testExportIpynb "codedown-exporter-nbconvert-rst" "rst"
    it "codedown-exporter-nbconvert-markdown" $ testExportIpynb "codedown-exporter-nbconvert-markdown" "md"
    -- it "codedown-exporter-slides" $ testExport "codedown-exporter-slides" "slides.html"


nbconvertConfig :: Text -> Text
nbconvertConfig scheme = [__i|exporters.nbconvert.enable = true;
                              exporters.nbconvert.texliveScheme = "#{scheme}";|]

pandocConfig :: Text
pandocConfig = [__i|exporters.pandoc.enable = true;|]

typstConfig :: Text
typstConfig = [__i|exporters.typst.enable = true;|]
