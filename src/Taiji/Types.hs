{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module Taiji.Types where

import           Bio.Data.Bed           (BED)
import           Bio.Pipeline.Instances ()
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Binary            (Binary (..))
import           Data.Binary.Orphans    ()
import qualified Data.ByteString.Char8  as B
import           Data.CaseInsensitive   (CI)
import           Data.Default.Class
import qualified Data.Map.Strict        as M
import qualified Data.Matrix.Unboxed    as MU
import qualified Data.Text              as T
import qualified Data.Vector            as V
import           Data.Vector.Binary     ()
import           GHC.Generics           (Generic)

data RankTable = RankTable
    { rowNames    :: V.Vector T.Text
    , colNames    :: V.Vector T.Text
    , ranks       :: MU.Matrix Double
    , expressions :: Maybe (MU.Matrix Double)
    } deriving (Generic)

instance Binary RankTable
instance FromJSON RankTable
instance ToJSON RankTable

data TaijiResults = TaijiResults
    { ranktable :: RankTable
    , nets      :: M.Map T.Text (M.Map T.Text T.Text)
    } deriving (Generic)

instance Binary TaijiResults
instance FromJSON TaijiResults
instance ToJSON TaijiResults

data TaijiConfig = TaijiConfig
    { _taiji_output_dir   :: FilePath
    , _taiji_input        :: FilePath
    , _taiji_picard       :: Maybe FilePath
    , _taiji_genome       :: Maybe FilePath
    , _taiji_bwa_index    :: Maybe FilePath
    , _taiji_star_index   :: Maybe FilePath
    , _taiji_annotation   :: Maybe FilePath
    , _taiji_rsem_index   :: Maybe FilePath
    , _taiji_genome_index :: Maybe FilePath
    , _taiji_motif_file   :: Maybe FilePath
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
        , _taiji_genome_index = def
        , _taiji_motif_file = def
        }

instance ToJSON TaijiConfig where
    toJSON = genericToJSON defaultOptions
        { fieldLabelModifier = drop 7 }
    toEncoding = genericToEncoding defaultOptions
        { fieldLabelModifier = drop 7 }

instance FromJSON TaijiConfig where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = drop 7 }

data NetNode = NetNode
    { nodeName      :: CI B.ByteString
    , nodeExpression :: Maybe Double
    , nodeScaledExpression :: Maybe Double
    , pageRankScore :: Maybe Double
    } deriving (Generic, Show, Read)

data NetEdge = NetEdge
    { weightExpression  :: Maybe Double
    , weightCorrelation :: Maybe Double
    , sites             :: [BED]
    } deriving (Generic, Show, Read)

instance Binary NetNode
instance Binary NetEdge
