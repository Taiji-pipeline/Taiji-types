{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Taiji.Types where

import           Bio.Data.Bed
import           Bio.Pipeline.Instances ()
import           Bio.Pipeline.Utils     (Directory)
import           Data.Aeson
import qualified Data.ByteString.Char8  as B
import           Data.CaseInsensitive   (CI)
import           Data.Default.Class
import           Data.Serialize         (Serialize (..))
import           GHC.Generics           (Generic)

data TaijiConfig = TaijiConfig
    { _taiji_output_dir   :: Directory
    , _taiji_input        :: FilePath
    , _taiji_picard       :: Maybe FilePath
    , _taiji_genome       :: Maybe FilePath
    , _taiji_bwa_index    :: Maybe FilePath
    , _taiji_star_index   :: Maybe FilePath
    , _taiji_annotation   :: Maybe FilePath
    , _taiji_rsem_index   :: Maybe FilePath
    , _taiji_genome_index :: Maybe FilePath
    , _taiji_motif_file   :: Maybe FilePath
    , _taiji_tmp_dir      :: Maybe FilePath
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
        , _taiji_tmp_dir = def
        }

-- Drop "_taiji_" prefix
instance ToJSON TaijiConfig where
    toJSON = genericToJSON defaultOptions
        { fieldLabelModifier = drop 7 }
    toEncoding = genericToEncoding defaultOptions
        { fieldLabelModifier = drop 7 }

-- Drop "_taiji_" prefix
instance FromJSON TaijiConfig where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = drop 7 }

type GeneName = CI B.ByteString
type Promoter = BEDExt BED3 GeneName
type RegDomain = BEDExt BED3 GeneName

data DomainType = Promoter
                | Enhancer
                deriving (Eq)

type Linkage = (GeneName, [(GeneName, Double)], [(GeneName, Double)])

data NetNode = NetNode
    { nodeName             :: GeneName
    , nodeExpression       :: Maybe Double
    , nodeScaledExpression :: Maybe Double
    , pageRankScore        :: Maybe Double
    , pageRankPvalue       :: Maybe Double
    } deriving (Generic, Show, Read, Eq, Ord)

instance Serialize NetNode

defaultNode :: NetNode
defaultNode = NetNode
    { nodeName = ""
    , nodeExpression = Nothing
    , nodeScaledExpression = Nothing
    , pageRankScore = Nothing
    , pageRankPvalue = Nothing
    }

data NetEdge = NetEdge
    { weightExpression  :: Maybe Double
    , weightCorrelation :: Maybe Double
    , sites             :: Double
    } deriving (Generic, Show, Read)

instance Serialize NetEdge

defaultEdge :: NetEdge
defaultEdge = NetEdge
    { weightExpression = Nothing 
    , weightCorrelation = Nothing
    , sites = 0
    }

instance Default (CI B.ByteString) where
    def = ""
