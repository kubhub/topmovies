module Database where

import DownloadMovies
import MovieDataType
import ParseResponse
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.List
import Control.Exception

-- | The below functions are used for establishing a connection to the database
dbConnect :: IO Connection
dbConnect = connectSqlite3 "DB.db"

r :: (Connection -> IO a) -> IO a
r = bracket dbConnect disconnect


-- | setupDatabase initialises the database creating tables
setupDatabase :: IO Connection
setupDatabase = do
   conn <- dbConnect
   print "Connection Successful"
   run conn "CREATE TABLE IF NOT EXISTS movieTable (\
                \id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
                \rank INTEGER NOT NULL UNIQUE ,\
                \name VARCHAR(80) NOT NULL ,\
                \year INTEGER NOT NULL ,\
                \rating REAL NOT NULL,\
                \dirFName VARCHAR(40) NOT NULL,\
                \dirLName VARCHAR(40) NOT NULL)" []
   run conn "CREATE TABLE IF NOT EXISTS starringTable (\
                \id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, \
                \starFName VARCHAR(40) NOT NULL, \
                \starLName VARCHAR(40) NOT NULL)" []
   run conn "CREATE TABLE IF NOT EXISTS movTable_starTable (\
                \movieID INTEGER, \
                \starringID INTEGER, \
                \FOREIGN KEY(movieID) REFERENCES movieTable(id), \
                \FOREIGN KEY(starringID) REFERENCES starringTable(id) )" []
   commit conn

   return conn
-- | populateMovies populates movie data into a table of its own.
-- | Begins by preparing Connections
-- | Then inserts and Commit
populateMovies :: IO ()
populateMovies = do
     movieDataset <- readMovies
     conn <- dbConnect
     stmt <- prepare conn "INSERT OR IGNORE INTO movieTable(\
                                    \rank,\
                                    \name,\
                                    \year,\
                                    \rating,\
                                    \dirFName,\
                                    \dirLName)\
                                    \VALUES (?,?,?,?,?,?)"

     stmt2 <- prepare conn "INSERT OR IGNORE INTO starringTable (\
                             \starFName, \
                             \starLName) \
                             \VALUES (?,?)"
     stmt3 <- prepare conn "INSERT OR IGNORE INTO movTable_starTable (\
                                \movieID, \
                                \starringID) \
                                \VALUES (?,?)"

     let insertMovies a = [toSql (rank $ movieDataset !! (a-1)),
                           toSql (name $ movieDataset !! (a-1)),
                           toSql (year $ movieDataset !!(a-1)),
                           toSql (rating $ movieDataset !!(a-1)),
                           toSql (takeWhile (/= ' ') . director $ movieDataset !!(a-1)),
                           toSql (reverse . takeWhile (/= ' ') . reverse . director $ movieDataset !!(a-1))]

     let insertActors a = [toSql (takeWhile (/= ' ') . starring $ movieDataset!!(a-1)),
                            toSql (reverse .  takeWhile (/= ' ') . reverse . starring $ movieDataset!!(a-1))]

     let insertActorsMovies a = [toSql ((a-1)::Int), toSql ((a-1)::Int) ]
     let populateMovieTable = map insertMovies [1..152]
     let populateActors = map insertActors [1..152]
     let populateActorsMovies = map insertActorsMovies [1..152]

     executeMany stmt populateMovieTable
     print "Movies inserted successfully"
     commit conn
     executeMany stmt2 populateActors
     print "Stars inserted successfully"
     commit conn
     executeMany stmt3 populateActorsMovies
     print "All Successful"
     commit conn
