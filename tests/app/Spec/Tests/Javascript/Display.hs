{-# LANGUAGE OverloadedStrings #-}

module Spec.Tests.Javascript.Display (tests) where

import qualified Data.Aeson as A
import qualified Data.Map as M
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Vector as V
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.JupyterTypes


-- | codedown-d3 is added to the environment whenever d3 is selected. Its static path renders
-- with jsdom in the kernel; its interactive path emits the d3 bundle plus the cell's drawing
-- code as a script, for the frontend to run.
tests :: (HasJupyterRunnerContext context, JupyterRunnerMonad m) => SpecFree context m ()
tests = describe "D3 display" $ do
  it "renders a static chart to SVG" $
    displayDatasShouldSatisfy "javascript" staticCode $ \datas ->
      case htmlOutputs datas of
        [html] -> do
          html `textShouldContain` "<svg"
          html `textShouldContain` "<circle"
        xs -> expectationFailure [i|Expected one text/html output, got: #{xs}|]

  it "emits an interactive chart with the bundle inlined" $
    displayDatasShouldSatisfy "javascript" interactiveCode $ \datas ->
      case htmlOutputs datas of
        [html] -> do
          html `textShouldContain` "codedown-d3-"
          html `textShouldContain` "<script>"
          -- The d3 bundle is inlined rather than fetched, so the output works offline and
          -- inside a sandboxed frame.
          html `textShouldContain` "d3js.org"
        xs -> expectationFailure [i|Expected one text/html output, got: #{xs}|]

-- Jupyter stores a multi-line MIME payload as an array of lines, so flatten before matching.
htmlOutputs :: [M.Map MimeType A.Value] -> [T.Text]
htmlOutputs datas = [flatten v | Just v <- fmap (M.lookup (MimeType "text/html")) datas]
  where
    flatten (A.String t) = t
    flatten (A.Array xs) = T.concat [t | A.String t <- V.toList xs]
    flatten _ = ""

staticCode :: T.Text
staticCode = [__i|const { renderStatic } = require("codedown-d3");
                  renderStatic((d3, el, data, size) => {
                    d3.select(el).append("svg")
                      .attr("width", size.width).attr("height", size.height)
                      .append("circle").attr("cx", 20).attr("cy", 20).attr("r", 10);
                  }, [1, 2, 3], { width: 100, height: 50 });|]

interactiveCode :: T.Text
interactiveCode = [__i|const { render } = require("codedown-d3");
                       render((d3, el, data, size) => {
                         d3.select(el).append("svg")
                           .attr("width", size.width).attr("height", size.height);
                       }, [1, 2, 3], { width: 100, height: 50 });|]
