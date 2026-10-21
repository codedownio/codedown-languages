{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Spec.Tests.Lua.Display (tests) where

import qualified Data.Aeson as A
import qualified Data.Map as M
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Test.Sandwich as Sandwich
import TestLib.JupyterRunnerContext
import TestLib.JupyterTypes


-- | xeus-lua's rich output lives on the `ilua.display` table, which it defines in Lua on
-- top of a `_display_data` binding (src/xdisplay.cpp). Each of these publishes a
-- display_data message with one MIME type; a kernel that only ever manages stdout isn't
-- much of a notebook kernel, so they're worth pinning per MIME type.
tests :: (HasJupyterRunnerContext context, JupyterRunnerMonad m) => Text -> SpecFree context m ()
tests kernel = describe "Display" $ do
  it "publishes text/html" $
    displayDatasShouldSatisfy kernel [i|ilua.display.html("<b>hi</b>")|] $ \datas ->
      case mimeOutputs "text/html" datas of
        [html] -> html `shouldBe` "<b>hi</b>"
        xs -> expectationFailure [i|Expected one text/html output, got: #{xs}|]

  it "publishes text/latex" $
    displayDatasShouldSatisfy kernel [i|ilua.display.latex("$x^2$")|] $ \datas ->
      case mimeOutputs "text/latex" datas of
        [latex] -> latex `shouldBe` "$x^2$"
        xs -> expectationFailure [i|Expected one text/latex output, got: #{xs}|]

  it "publishes text/plain" $
    displayDatasShouldSatisfy kernel [i|ilua.display.plain_text("just text")|] $ \datas ->
      case mimeOutputs "text/plain" datas of
        [text] -> text `shouldBe` "just text"
        xs -> expectationFailure [i|Expected one text/plain output, got: #{xs}|]

  -- The HTML goes through ilua.json's string encoder on the way out, so anything that
  -- would break the JSON payload has to survive the round trip.
  it "escapes a payload containing quotes and newlines" $
    displayDatasShouldSatisfy kernel [__i|ilua.display.html('<p class="x">a\\nb</p>')|] $ \datas ->
      case mimeOutputs "text/html" datas of
        [html] -> do
          html `textShouldContain` [i|class="x"|]
          html `textShouldContain` "a\nb"
        xs -> expectationFailure [i|Expected one text/html output, got: #{xs}|]

  it "publishes several outputs from one cell, in order" $
    displayDatasShouldSatisfy kernel [__i|ilua.display.html("<i>first</i>")
                                          ilua.display.html("<i>second</i>")|] $ \datas ->
      mimeOutputs "text/html" datas `shouldBe` ["<i>first</i>", "<i>second</i>"]

-- Jupyter stores a multi-line MIME payload as an array of lines, so flatten before matching.
mimeOutputs :: Text -> [M.Map MimeType A.Value] -> [Text]
mimeOutputs mime datas = [flatten v | Just v <- fmap (M.lookup (MimeType mime)) datas]
  where
    flatten (A.String t) = t
    flatten (A.Array xs) = T.concat [t | A.String t <- V.toList xs]
    flatten _ = ""
