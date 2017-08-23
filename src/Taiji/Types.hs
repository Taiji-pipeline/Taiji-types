{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module Taiji.Types where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Binary           (Binary (..))
import qualified Data.ByteString.Char8 as B
import           Data.Default.Class
import qualified Data.Map.Strict       as M
import qualified Data.Matrix.Unboxed   as MU
import qualified Data.Text             as T
import qualified Data.Vector           as V
import           Data.Vector.Binary    ()
import           GHC.Generics          (Generic)

data RankTable = RankTable
    { rowNames    :: V.Vector B.ByteString
    , colNames    :: V.Vector T.Text
    , ranks       :: MU.Matrix Double
    , expressions :: MU.Matrix Double
    } deriving (Generic)

instance Binary (MU.Matrix Double)
instance Binary RankTable

data TaijiResults = TaijiResults
    { ranktable :: RankTable
    , nets      :: M.Map T.Text (M.Map B.ByteString B.ByteString)
    } deriving (Generic)

instance Binary TaijiResults

data TaijiConfig = TaijiConfig
    { _taiji_output_dir :: FilePath
    , _taiji_input      :: FilePath
    , _taiji_picard     :: Maybe FilePath
    , _taiji_genome     :: Maybe FilePath
    , _taiji_bwa_index  :: Maybe FilePath
    , _taiji_star_index :: Maybe FilePath
    , _taiji_annotation :: Maybe FilePath
    , _taiji_rsem_index :: Maybe FilePath
    } deriving (Generic)

instance Default TaijiConfig where
    def = TaijiConfig
        { _taiji_output_dir = "output"
        , _taiji_input = "input.yml"
        , _taiji_picard = def
        , _taiji_genome = def
        , _taiji_bwa_index = def
        , _taiji_star_index = def
        , _taiji_annotation = def
        , _taiji_rsem_index = def
        }

instance ToJSON TaijiConfig where
    toEncoding = genericToEncoding defaultOptions
        { fieldLabelModifier = ("_taiji_" ++) }

instance FromJSON TaijiConfig where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = drop 7 }
