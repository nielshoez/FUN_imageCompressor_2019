--
-- EPITECH PROJECT, 2020
-- parse
-- File description:
-- Parse
--

module Parse
    (getHelp
    ) where

import System.IO  
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Ix
import Text.Read
import System.Random
import Lib

spliter :: String -> [String]
spliter str = words str

removePunc xs = [ x | x <- xs, not (x `elem` ",\n") ]

tailInit :: String -> String
tailInit "" = ""
tailInit str = tail $ init str

listToTuple :: [String] -> (Int, Int, Int)
listToTuple tab = (read $ tab !! 0, read $ tab !! 1, read $ tab !! 2)

listToTuple2 :: [String] -> (Int, Int)
listToTuple2 tab = (read $ tab !! 0, read $ tab !! 1)

comaSpace :: String -> String
comaSpace str = let
        repl ',' = ' '
        repl  c   = c
    in  map repl str

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

stringToTuple :: String -> String -> [((Int, Int), (Int, Int, Int))] -> [((Int, Int), (Int, Int, Int))]
stringToTuple [] [] tupleList = tupleList 
stringToTuple str1 str2 tupleList = do
                        tupleList ++ [((listToTuple2 $ spliter $ comaSpace $ tailInit str1), (listToTuple $ spliter $ comaSpace $ tailInit str2))]

tabToTuple :: [String] -> [((Int, Int), (Int, Int, Int))] -> [((Int, Int), (Int, Int, Int))]
tabToTuple [] list = list
tabToTuple (x:xs) list = do
                    let first = spliter x
                    let second = first !! 0
                    let third = first !! 1
                    tabToTuple xs (stringToTuple second third list)

isHelp :: [String] -> Bool
isHelp [] = False
isHelp (x:xs) = case x of
                    "-h" -> True
                    _ -> False

getHelp :: [String] -> Int -> Float -> IO ()
getHelp args color convergence
        | isHelp args = do
            help <- readFile "./README.md"
            putStrLn help
        | otherwise = do 
            content <- readFile $ args !! 2
            gen <- newStdGen
            let coords = tabToTuple (wordsWhen (=='\n') content) []
            let randTab = Lib.getAllRand color coords [] gen
            let assosTab = Lib.calcMinDistance coords randTab 0 []
            let centroidList = Lib.getAllClusters assosTab randTab coords []
            let testAverage = Lib.getAllAverage centroidList []
            Lib.kMeansLoop randTab testAverage coords convergence
