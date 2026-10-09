-- | Checking that a language server really formats.
--
-- Advertising @documentFormattingProvider@ proves nothing: a server will happily offer it and
-- then fail every request because the tool behind it isn't installed, which is what
-- bash-language-server did until shfmt was added to its wrapper. So these send a real
-- @textDocument/formatting@ and look at what comes back.

module TestLib.LSP.Formatting (
  itFormatsAs
  , itFormatsAs'
  , itReformats

  , formatsAs
  ) where

import Control.Monad.Reader
import Data.String.Interpolate
import Data.Text (Text)
import Language.LSP.Protocol.Types
import Language.LSP.Test
import Language.LSP.Test.Helpers (LspContext, withLspSession)
import Test.Sandwich as Sandwich
import Test.Sandwich.Waits (waitUntil)
import TestLib.LSP.Session
import TestLib.Types


-- | Format a document and check the result against the text it should become.
itFormatsAs :: (
  LspContext ctx m, HasNixEnvironment ctx
  ) => Text -> FilePath -> LanguageKind -> Text -> Text -> SpecFree ctx m ()
itFormatsAs = itFormatsAs' defaultTimeout

-- | 'itFormatsAs' with an explicit timeout, for servers that need to load a project first.
itFormatsAs' :: (
  LspContext ctx m, HasNixEnvironment ctx
  ) => Double -> Text -> FilePath -> LanguageKind -> Text -> Text -> SpecFree ctx m ()
itFormatsAs' timeoutSeconds name filename languageKind code expected =
  itFormats' timeoutSeconds name filename languageKind code (`shouldBe` expected)

-- | Weaker than 'itFormatsAs': only that formatting rewrote the document. For formatters whose
-- exact output we haven't pinned down, this still catches a server that advertises formatting
-- and can't deliver it.
itReformats :: (
  LspContext ctx m, HasNixEnvironment ctx
  ) => Text -> FilePath -> LanguageKind -> Text -> SpecFree ctx m ()
itReformats name filename languageKind code =
  itFormats' defaultTimeout name filename languageKind code (`shouldNotBe` code)

-- | The body of 'itFormatsAs', for specs that need to wrap it (e.g. to mark it 'pending').
formatsAs :: (
  LspContext ctx m, HasNixEnvironment ctx
  ) => Double -> Text -> FilePath -> LanguageKind -> Text -> Text -> ExampleT ctx m ()
formatsAs timeoutSeconds name filename languageKind code expected =
  formats' timeoutSeconds name filename languageKind code (`shouldBe` expected)

itFormats' :: (
  LspContext ctx m, HasNixEnvironment ctx
  ) => Double -> Text -> FilePath -> LanguageKind -> Text -> (Text -> ExampleT ctx m ()) -> SpecFree ctx m ()
-- Deliberately keeps the code out of the test name: Sandwich names the test's directory after
-- it, and the escaped newlines turn into backslashes in a path, which R.cache rewrites to
-- forward slashes and then can't write to.
itFormats' timeoutSeconds name filename languageKind code cb =
  it [i|#{name} formats #{filename}|] $
    formats' timeoutSeconds name filename languageKind code cb

formats' :: (
  LspContext ctx m, HasNixEnvironment ctx
  ) => Double -> Text -> FilePath -> LanguageKind -> Text -> (Text -> ExampleT ctx m ()) -> ExampleT ctx m ()
formats' timeoutSeconds name filename languageKind code cb = do
  lspSessionOptions <- lspSessionOptionsFor name filename languageKind code

  withLspSession lspSessionOptions $ \_ -> do
    ident <- openDoc filename languageKind
    -- formatDoc applies the edits to the session's copy of the document, so retrying after
    -- the server has warmed up is harmless: a formatted document just comes back unchanged.
    waitUntil timeoutSeconds $ do
      formatDoc ident formattingOptions
      documentContents ident >>= lift . cb

defaultTimeout :: Double
defaultTimeout = 180

formattingOptions :: FormattingOptions
formattingOptions = FormattingOptions 2 True Nothing Nothing Nothing
