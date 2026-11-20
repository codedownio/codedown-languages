{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Spec.Tests.Exporters (tests) where

import Control.Monad (forM_)
import Data.String.Interpolate
import Data.Text (Text)
import Language.LSP.Protocol.Types (LanguageKind(..))
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
    it "codedown-exporter-pandoc-beamer" $ testExportMd "codedown-exporter-pandoc-beamer" "pdf"
    it "codedown-exporter-pandoc-pdf" $ testExportMd "codedown-exporter-pandoc-pdf" "pdf"

    it "pandoc-export-server renders live" $
      testExportServe "pandoc-export-server" "input.md" LanguageKind_Markdown sampleMdFile "html" [("to", "slidy")] []

    it "pandoc-export-server locates a line in the output and back" $
      testExportServeLocate "pandoc-export-server" "input.md" LanguageKind_Markdown sampleMdFile [("to", "pdf")] [] 2

    it "pandoc-export-server locates a word of every kind and back" $
      testExportServeLocateWords "pandoc-export-server" "input.md" LanguageKind_Markdown sampleMdWordsFile [("to", "pdf"), ("from", "commonmark_x+tex_math_gfm+implicit_figures")] [("pic.png", tinyPng)] sampleMdWords

  forM_ ["xelatex", "lualatex" :: Text] $ \engine ->
    introduceNixEnvironment [] [pandocConfig, [i|exporters.pandoc.engine = "#{engine}";|]] [i|Exporters (pandoc, #{engine})|] $ do
      it "codedown-exporter-pandoc-pdf" $ testExportMd "codedown-exporter-pandoc-pdf" "pdf"

      it "pandoc-export-server locates a word of every kind and back" $
        testExportServeLocateWords "pandoc-export-server" "input.md" LanguageKind_Markdown sampleMdWordsFile [("to", "pdf"), ("from", "commonmark_x+tex_math_gfm+implicit_figures")] [("pic.png", tinyPng)] sampleMdWords

  introduceNixEnvironment [] [latexConfig] [i|Exporters (latex)|] $ do
    it "codedown-exporter-latex" $ testExportLatex "codedown-exporter-latex" "pdf"

    it "latex-export-server renders live" $
      testExportServe "latex-export-server" "input.tex" LanguageKind_LaTeX sampleLatexFile "pdf" [] [("out.txt", "line one\n")]

    it "latex-export-server locates a line in the output and back" $
      testExportServeLocate "latex-export-server" "input.tex" LanguageKind_LaTeX sampleLatexFile [] [("out.txt", "line one\n")] 3

  introduceNixEnvironment [] [typstConfig] [i|Exporters (typst)|] $ do
    it "codedown-exporter-typst" $ testExportTypst "codedown-exporter-typst" "pdf"

    it "typst-export-server renders live" $
      testExportServe "typst-export-server" "input.typ" (LanguageKind_Custom "typst") sampleTypstFile "pdf" [] []


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

latexConfig :: Text
latexConfig = [__i|exporters.latex.enable = true;|]

typstConfig :: Text
typstConfig = [__i|exporters.typst.enable = true;|]
