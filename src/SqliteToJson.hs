{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass  #-}
module SqliteToJson where
import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Text.Lazy.IO as I
import Database
import Database.HDBC
import Database.HDBC.Sqlite3
import MovieDataType
import Control.Exception

-- | Below functions are used to read the json file
jsonFile :: FilePath
jsonFile = "DB.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

data Movies = Movies {
      id :: Int,            --Record id
      rank :: Int,          --Movie Rank
      name :: String,       --Movie name
      year :: Int,          --Year produced
      rating :: Int,     --Rating by rottentomatoes
      dirFName :: String,   --D. First Name
      dirLName :: String    --D. Last Name
      } deriving (Eq, Generic, Show)

-- | Fefault version is defined for use of toJSON to encode the data
instance ToJSON Movies where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON Starring where
    toEncoding = genericToEncoding defaultOptions

-- | Reads each row from the movie table
movTableRows :: [SqlValue] -> Movies
movTableRows l = Movies (fromSql $ l !! 0)
                       (fromSql $ l !! 1)
                       (fromSql $ l !! 2)
                       (fromSql $ l !! 3)
                       (fromSql $ l !! 4)
                       (fromSql $ l !! 5)
                       (fromSql $ l !! 6)

-- | Reads each row from the starring table
starTableRows :: [SqlValue] -> Starring
starTableRows l = Starring (fromSql $ l !! 0)
                           (fromSql $ l !! 1)
                           (fromSql $ l !! 2)
-- | Fetches all rows into Strings
convMovieToJson :: IConnection conn => conn -> IO [Movies]
convMovieToJson conn = do
   stmt <- prepare conn "SELECT * FROM movieTable LIMIT 152"
   execute stmt []
   stringRows      <- fetchAllRows' stmt
   return $ map movTableRows stringRows

convStarringToJson :: IConnection conn => conn -> IO [Starring]
convStarringToJson conn = do
  stmt <- prepare conn "SELECT * FROM starringTable LIMIT 152"
  execute stmt []
  stringRows      <- fetchAllRows' stmt
  return $ map starTableRows stringRows

-- | A do block to encode and dump the database to JSON
generateJson = do
                movieTable <- r convMovieToJson
                starringTable <- r convStarringToJson
                let encodeSqlite = encodeFile jsonFile (movieTable, starringTable)
                encodeSqlite
