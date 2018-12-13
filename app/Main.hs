module Main where
import SqliteToJson
import DownloadMovies
import DatabaseQueries
import ParseResponse
import Database
import System.Environment
import Control.Monad
import Data.Aeson

-- | Build the database and populate the data
populate = do
   downloadPage
   dropDatabase

   setupDatabase
   populateMovies

-- | Main module offering the user a list of options
main = do
       populate
       let loop = do
                   putStrLn "\n2018 Movies App command [args]\n"
                   putStrLn "\nPlease select an option:\n"
                   putStrLn "1: Display movies by starring actor name"
                   putStrLn "2: Display top 10 starring actors"
                   putStrLn "3: Display rating of a particular movie"
                   putStrLn "4: Delete record based on a given movie name"
                   putStrLn "5: Update a record based on a given movie name"
                   putStrLn "6: Display rank of a movie"
                   putStrLn "7: Show starring actors for a particular movie"
                   putStrLn "8: Dump database to JSON"
                   putStrLn "Type -1 to exit..\n"

                   putStrLn "Option: "
                   option <- getLine
                   print $ "You selected option " ++ option ++ "\n"
                   case option of "1" -> do
                                         putStrLn "Type Actors name e.g Nicolas Cage"
                                         name <- getLine
                                         let outStr = movieByActorName name
                                         outStr

                                  "2" -> do actorFrequency 10
                                  "3" -> do
                                         putStrLn "Type a movie name e.g Overlord"
                                         name <- getLine
                                         let outStr = ratingByMovieName name
                                         outStr
                                  "4" -> do
                                          putStrLn "Type a movie name e.g Mountain"
                                          name <- getLine
                                          let outStr = delByMovieName name
                                          outStr
					  print $ name ++ " deleted"
                                  "5" -> do
                                          putStrLn "Type a movie name e.g Mandy"
                                          name <- getLine
                                          putStrLn "Type new rating to update rank e.g. 75"
                                          userRating <- getLine
                                          updByMovieName userRating name
					  print $ "The rating for " ++ name ++ " has been changed to " ++ userRating
                                  "6" -> do
                                          putStrLn "Type a movie name e.g Upgrade"
                                          name <- getLine
                                          let outStr = rankByMovieName name
                                          outStr
                                          print $ "out of 152"

                                  "7" -> do
                                          putStrLn "Type a movie name e.g Blaze"
                                          name <- getLine
                                          let outStr = starringByMovieName name
                                          outStr
                                  "8" -> do
                                          generateJson
                   when (option /= "-1") loop



       loop
       putStrLn "Program Terminated!"
