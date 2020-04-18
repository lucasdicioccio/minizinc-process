{-# LANGUAGE OverloadedStrings #-}

-- | Provides primitives for inspecting the interface of a model.
module Process.Minizinc.Inspect
  ( inspect
  , TypeInfo(..)
  , TypeDeclarations
  , Method(..)
  , Interface(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map (Map)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LByteString
import System.Process.ByteString (readProcessWithExitCode)

-- | Name of variables.
type Name = Text

-- | Information regarding one name.
data TypeInfo = TypeInfo {
    _type :: Text
  , _set :: Bool
  , _dim :: Maybe Int
  } deriving (Show)

instance FromJSON TypeInfo where
  parseJSON = withObject "TypeInfo" $ \v -> TypeInfo
    <$> v .: "type"
    <*> v .:? "set" .!= False
    <*> v .:? "dim"

-- | Type declarations of the minizinc model.
type TypeDeclarations = Map Name TypeInfo

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
data Interface = Interface 
  { _method :: Method
  , _has_output_item :: Bool
  , _input :: TypeDeclarations
  , _output :: TypeDeclarations
  } deriving (Show)

instance FromJSON Interface where
  parseJSON = withObject "Interface" $ \v -> Interface
    <$> v .: "method"
    <*> v .: "has_output_item"
    <*> v .: "input"
    <*> v .: "output"

-- | Calls the minizinc binary to output the model interface.
inspect :: FilePath -> IO (Maybe Interface)
inspect path = do
  (_, out, err) <- readProcessWithExitCode "minizinc" args ""
  seq (ByteString.length err) $ pure $ decode $ LByteString.fromStrict out
  where
    args = [ "-c", "--model-interface-only", path ]
