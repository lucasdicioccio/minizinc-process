{-# LANGUAGE OverloadedStrings #-}

-- | Provides primitives for inspecting the interface of a model.
module Process.Minizinc.Inspect
  ( inspect,
    TypeInfo (..),
    TypeDeclarations,
    Method (..),
    Interface (..),
  )
where

import Control.Applicative ((<|>))
import Data.Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LByteString
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.List as List
import System.Process.ByteString (readProcessWithExitCode)

-- | Name of variables.
type Name = Text

-- | Information regarding one name.
data TypeInfo
  = TypeInfo
      { _type :: Text,
        _set :: Bool,
        _dim :: Maybe Int
      }
  deriving (Eq, Ord, Show)

instance FromJSON TypeInfo where
  parseJSON = withObject "TypeInfo" $ \v ->
    TypeInfo
      <$> v .: "type"
      <*> v .:? "set" .!= False
      <*> v .:? "dim"

-- | Type declarations of the minizinc model.
type TypeDeclarations = Map Name TypeInfo

haskellify :: TypeDeclarations -> Maybe Text
haskellify typedecls =
    fmap Text.unlines $ sequence $ fmap property $ pairs
  where
    pairs :: [(Name, TypeInfo)]
    pairs = List.sort $ Map.assocs typedecls

    property :: (Name, TypeInfo) -> Maybe Text
    property (name, TypeInfo "int" False Nothing)    = Just $ mconcat [ name, "::", "Int" ]
    property (name, TypeInfo "float" False Nothing)  = Just $ mconcat [ name, "::", "Float" ]
    property (name, TypeInfo "bool" False Nothing)  = Just $ mconcat [ name, "::", "Bool" ]
    property (name, TypeInfo "int" False (Just n))   = Just $ mconcat [ name, "::", wrapAry n "Int" ]
    property (name, TypeInfo "float" False (Just n)) = Just $ mconcat [ name, "::", wrapAry n "Float" ]
    property (name, TypeInfo "bool" False (Just n)) = Just $ mconcat [ name, "::", wrapAry n "Bool" ]
    property _                                       = Nothing

    wrapAry :: Int -> Text -> Text
    wrapAry n str =
        let lparens = Text.replicate n "["
            rparens = Text.replicate n "]"
        in lparens <> str <> rparens

-- | Optimization method.
data Method = Minimize | Maximize | Satisfy
  deriving (Show)

instance FromJSON Method where
  parseJSON = withText "Method" $ \s -> case s of
    "max" -> pure Maximize
    "min" -> pure Minimize
    "sat" -> pure Satisfy
    v -> fail $ "unsupported method: " ++ Text.unpack v

-- | A description of the model input/output.
data Interface
  = Interface
      { _method :: Method,
        _has_output_item :: Bool,
        _input :: TypeDeclarations,
        _output :: TypeDeclarations
      }
  deriving (Show)

instance FromJSON Interface where
  parseJSON = withObject "Interface" $ \v ->
    Interface
      <$> v .: "method"
      <*> (v .: "has_output_item" <|> v.: "has_outputItem")
      <*> v .: "input"
      <*> v .: "output"

-- | Calls the minizinc binary to output the model interface.
inspect :: FilePath -> IO (Maybe Interface)
inspect path = do
  (_, out, err) <- readProcessWithExitCode "minizinc" args ""
  seq (ByteString.length err) $ pure $ decode $ LByteString.fromStrict out
  where
    args = ["-c", "--model-interface-only", path]
