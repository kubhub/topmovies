module DatabaseQueries where

import DownloadMovies
import MovieDataType
import ParseResponse
import Database
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.List


-- | Use of JOIN
-- | Show all starring actors for a particular movie.
starringByMovieName movieName = do
   conn <- dbConnect
   r <- quickQuery' conn ("SELECT GROUP_CONCAT(DISTINCT (starFName ||' '|| starLName)) " ++ 
			 "AS aName FROM movieTable JOIN movTable_starTable ON " ++
			 "movieTable.id = movTable_starTable.movieID JOIN starringTable " ++  
			 "ON starringTable.id = movTable_starTable.starringID where  movieTable.name = ?") [toSql (movieName::String)]
   let rows = map convStars r
   mapM_ putStrLn rows
   disconnect conn

    where convStars :: [SqlValue] -> String
          convStars [s] =
             show starring
             where starring = (fromSql s)::String
		   star = case fromSql s of
                             Just x -> x
                             Nothing -> "NULL"
          convStars x = fail $ "Unexpected result: " ++ show x


-- | Use of DELETE.
-- | Delete a record based on a given movie name
delByMovieName movieName = do
   conn <- dbConnect
   r <- quickQuery' conn "DELETE FROM movieTable WHERE name = ? " [toSql (movieName::String)]
   commit conn
   disconnect conn


-- | Use of UPDATE.
-- | Update a record based on a given movie name
updByMovieName movieName userRating = do
   conn <- dbConnect
   r <- quickQuery' conn "UPDATE movieTable SET rating = ? WHERE name = ? " [toSql (movieName::String), toSql (userRating::String)]
   commit conn
   disconnect conn

-- | The following sql query returns the movie name based on the starring actor.
movieByActorName actorName = do
   conn <- dbConnect
   r <- quickQuery' conn ("SELECT name FROM movieTable,starringTable,movTable_starTable " ++
			 "WHERE movieTable.id = movTable_starTable.movieID AND " ++
			 "starringTable.id = movTable_starTable.starringID AND starringTable.starFName = ? " ++
			 "AND starringTable.starLName = ? ") [toSql ((takeWhile (/= ' ') $ actorName)::String), toSql ((reverse . takeWhile (/= ' ') $ reverse $ actorName)::String)]
   -- Convert each row into a String
   let rows = map convName r
   -- Print the rows out
   mapM_ putStrLn rows
   -- Disconnect from the database
   disconnect conn
    where convName :: [SqlValue] -> String
          convName [n] =
             show name
             where name = (fromSql n)::String

-- | The following sql query returns the most frequent starring actors from the database.
actorFrequency limit = do
   conn <- dbConnect
   r <- quickQuery' conn ("SELECT starFName, starLName, COUNT ((starFName ||' '|| starLName)) " ++
			 "FROM movieTable,starringTable,movTable_starTable " ++
			 "WHERE movieTable.id = movTable_starTable.movieID " ++
			 "AND starringTable.id = movTable_starTable.starringID " ++
			 "AND (starringTable.starLName != '' AND starringTable.starFName != '' ) " ++
			 "GROUP BY starFName,starLName HAVING COUNT((starFName ||' '|| starLName)) > 0 " ++ 
			 "ORDER BY COUNT ((starFName ||' '|| starLName)) DESC LIMIT ?") [toSql (limit::Integer)]

   let rows = map convStarFreq r
   mapM_ putStrLn rows
   disconnect conn
     where convStarFreq :: [SqlValue] -> String
           convStarFreq [f,l,counter] =
               show (firstName ++ " " ++ lastName ++ ", " ++ allStars)
               where firstName = (fromSql f)::String
                     lastName = (fromSql l)::String
                     allStars = (fromSql counter)::String
		     star = case fromSql counter of
                               Just x -> x
                               Nothing -> "NULL"
           convStarFreq x = fail $ "Unexpected result: " ++ show x


-- | The following sql query returns the name of the director based on the movie name
dirByMovieName movieName = do
   conn <- dbConnect
   r <- quickQuery' conn "SELECT dirFName,dirLName FROM movieTable WHERE name = ? " [toSql (movieName::String)]
   let rows = map convDirName r
   mapM_ putStrLn rows
   disconnect conn
  where convDirName :: [SqlValue] -> String
        convDirName [f,l] =
            show (firstName ++ " " ++ lastName)
            where firstName = (fromSql f)::String
                  lastName = (fromSql l)::String

-- | The following sql query shows the rating based on the movie name.
ratingByMovieName movieName = do
   conn <- dbConnect
   r <- quickQuery' conn "SELECT rating FROM movieTable WHERE name = ? " [toSql (movieName::String)]
   let rows = map convRating r
   mapM_ putStrLn rows
   disconnect conn

    where convRating :: [SqlValue] -> String
          convRating [r] =
             show rating
             where rating = (fromSql r)::Double


-- | Show rank based on a movie name
rankByMovieName movieName = do
   conn <- dbConnect
   r <- quickQuery' conn "SELECT rank FROM movieTable where name = ?" [toSql (movieName::String)]
   let rows = map convRank r
   mapM_ putStrLn rows
   disconnect conn

    where convRank :: [SqlValue] -> String
          convRank [r] =
              show rank
              where rank = (fromSql r)::Integer
