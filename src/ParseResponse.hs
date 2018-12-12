module ParseResponse where

import System.IO
import qualified Data.ByteString.Char8 as B
import MovieDataType

-- | A function used to filter the information required from rotten tomatoes site.
parseMovies movieDetails = listOfMovies
   where
    unpackData = B.unpack movieDetails
    htmlLines = lines unpackData
    -- rowIndex takes the line from the HTML file where the countdown begins.
    -- This line is unique as it appears 152 times, one instance per movie.
    rowIndex s = (take 26 s) == "        <div id='row-index"
    -- This functions breaks the list as long as the elements meet the predicate
    listBreak :: (a -> Bool) -> [a] -> [[a]]
    listBreak pred [] = []
    listBreak pred [x] = [[x]]
    listBreak pred (x0:x1:xs) =
      if pred x1 then [x0]:(listBreak pred (x1:xs))
      else let (l1:ls) = listBreak pred (x1:xs) in ((x0:l1):ls)
    startingLine = tail $ (listBreak rowIndex htmlLines)

    getRanking = map head $ map (drop 15) startingLine
    rank = map (takeWhile (/= '<')) $ map tail
                                    $ map (dropWhile (/= '#')) getRanking
    getName = map head $ map (drop 11) startingLine
    name = map (takeWhile (/= '<')) $ map tail $ map (dropWhile (/= '>'))
                                    $ map (dropWhile (/= '\'')) getName
    getDir = map head $ map (drop 26) startingLine
    director = map (takeWhile (/= '<')) $ map tail $ map (dropWhile (/= '>'))
                                    $ map (dropWhile (/= 'f')) getDir
    getYear = map head $ map (drop 11) startingLine
    year = map (takeWhile (/= ')')) $ map (drop 1) $ map tail
                                    $ map (dropWhile (/= '>'))
                                    $ map tail $ map (dropWhile (/= '>'))
                                    $ map tail $ map (dropWhile (/= '>'))
                                    $ map tail $ map (dropWhile (/= '>'))
                                    $ map tail $ map (dropWhile (/= '>')) getYear
    getRating = map head $ map (drop 11) startingLine
    rating = map (takeWhile (/= '%')) $ map (drop 111)
                                    $ map tail $ map (dropWhile (/= '>'))
                                    $ map tail $ map (dropWhile (/= '>'))
                                    $ map tail $ map (dropWhile (/= '>'))
                                    $ map tail $ map (dropWhile (/= '>'))
                                    $ map tail $ map (dropWhile (/= '>')) getRating
    getStarring = map head $ map (drop 24) startingLine
    starring = map (takeWhile (/= '<')) $ map tail $ map (dropWhile (/= '>'))
                                    $ map tail $ map (dropWhile (/= '>'))
                                    $ map (dropWhile (/= ':')) getStarring

    -- | Convert String to Integer format
    convStrToInt :: [String] -> [Int]
    convStrToInt = map read
    -- | Convert String to Double format
    convStrToDou :: [String] -> [Double]
    convStrToDou = map read
    -- | create an instance of a movie
    movie a = ((((((((Movie $ convStrToInt rank!!(a-1))
                            $ name!!(a-1))
                            $ director!!(a-1))
                            $ convStrToInt year!!(a-1))
                            $ convStrToDou rating!!(a-1))
                            $ starring!!(a-1)) ))
    convertRank = convStrToInt rank
    listOfMovies = map movie convertRank
