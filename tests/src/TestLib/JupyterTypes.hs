{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module TestLib.JupyterTypes where

import Data.Aeson as A
import Data.Aeson.TH as A
import Data.Char (toLower)
import Data.Map
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics
import TestLib.Aeson


type Metadata = A.Object

newtype MimeType = MimeType Text
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (ToJSONKey, FromJSONKey, ToJSON, FromJSON)

data StreamType = Stdin | Stdout | Stderr
  deriving (Show, Eq, Read)
$(deriveJSON (A.defaultOptions { A.constructorTagModifier = fmap toLower }) ''StreamType)

newtype TextOrTextList = TextOrTextList { unTextOrTextList :: Text } deriving (Show, Eq)
instance ToJSON TextOrTextList where
  toJSON (TextOrTextList t) = A.String t
instance FromJSON TextOrTextList where
  parseJSON (A.String t) = return $ TextOrTextList t
  parseJSON (A.Array xs) | Prelude.all isString xs =
                           return $ TextOrTextList (T.intercalate "\n" [t | (A.String t) <- V.toList xs])
    where isString (A.String {}) = True
          isString _ = False
  parseJSON _ = fail "Couldn't parse TextOrTextList"

data CodeOutput = StreamOutput { streamOutputName :: StreamType
                               , streamOutputText :: TextOrTextList }
                | DisplayDataOutput { displayDataData :: Map MimeType A.Value
                                    , displayDataMetadata :: Metadata }
                | ExecuteResultOutput { executeResultExecutionCount :: Int
                                      , executeResultData :: Map MimeType A.Value
                                      , executeResultMetadata :: Metadata }
                | ErrorOutput { errorOutputEname :: Text
                              , errorOutputEvalue :: Text
                              , errorOutputTraceback :: [Text] }
  deriving (Show, Eq)
$(deriveJSON (A.defaultOptions {
                 fieldLabelModifier = toSnakeAndDropTwoWords
                 , constructorTagModifier = toSnake . T.unpack . T.dropEnd (T.length "Output") . T.pack
                 , sumEncoding = defaultTaggedObject { tagFieldName="output_type" }
                 })
   ''CodeOutput)

data JupyterText = JupyterTextLines [Text]
                 | JupyterTextBlock Text deriving (Show, Eq)
instance ToJSON JupyterText where
  toJSON (JupyterTextLines ls) = toJSON ls
  toJSON (JupyterTextBlock text) = toJSON text
instance FromJSON JupyterText where
  parseJSON (A.String t) = return $ JupyterTextBlock t
  parseJSON (A.Array items) = return $ JupyterTextLines [x | (A.String x) <- V.toList items]
  parseJSON _ = fail "Couldn't parse JupyterText"

data JupyterCell = MarkdownCell { markdownSource :: JupyterText
                                , markdownMetadata :: Metadata }
                 | CodeCell { codeMetadata :: Metadata
                            , codeExecutionCount :: Maybe Int
                            , codeSource :: JupyterText
                            , codeOutputs :: [CodeOutput]
                            } deriving (Show, Eq)
$(deriveJSON (A.defaultOptions {
                 fieldLabelModifier = toSnakeAndDropFirstWord
                 , constructorTagModifier = fmap toLower . T.unpack . T.dropEnd (T.length "Cell") . T.pack
                 , sumEncoding = defaultTaggedObject { tagFieldName = "cell_type" }
                 })
  ''JupyterCell)

data JupyterNotebook = JupyterNotebook {
  notebookMetadata :: A.Value -- Don't bother parsing for now
  , notebookNbformat :: Int
  , notebookNbformatMinor :: Int
  , notebookCells :: [JupyterCell]
  } deriving (Show, Eq)
$(deriveJSON toSnake1 ''JupyterNotebook)
