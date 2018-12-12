{-# LANGUAGE DeriveGeneric #-}
module MovieDataType where
import Data.Aeson
import System.IO
import GHC.Generics
import Control.Exception
-- | The custom data type consists of: name, rank, year, rating, starring (actors), director
data Movie = Movie {
      rank :: Int,          --Movie Rank
      name :: String,       --Movie name
      director :: String,   --Movie Director
      year :: Int,          --Year produced
      rating :: Double,     --Rating by rottentomatoes
      starring :: String    --Starring Actors
      } deriving (Eq, Generic, Show)

-- | The starring data type consists of: id, first name, last name
data Starring = Starring {
      id :: Int,            --id
      starFName :: String,  --First Name
      starLName :: String   --Last Name
      } deriving (Eq, Generic, Show)
