module DownloadMovies where

import MovieDataType
import ParseResponse
import Network.URI
import Network.HTTP.Conduit
import Data.Maybe
import Data.Either
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Control.Exception

-- | Download all movies from rotten tomatoes
downloadMovies :: String -> String -> IO()
downloadMovies name url = catch (do
					contents <- simpleHttp url
					putStrLn $ "Downloading file: " ++ url
					L.writeFile name contents)
				(\e -> case e of
				      	HttpExceptionRequest{} -> putStrLn "Http request failed"
				      	InvalidUrlException{} -> putStrLn "Bad URL")
-- | Saves the page as rottentomatoes.html
downloadPage :: IO()
downloadPage = do
    downloadMovies "rottentomatoes.html" "https://editorial.rottentomatoes.com/guide/best-movies-of-2018"
-- | Read the rotten tomatoes page into memory.
readMovies :: IO [Movie]
readMovies = do
    movieDetails <- B.readFile "rottentomatoes.html"
    let parsedData = parseMovies movieDetails
    return parsedData
