--
-- EPITECH PROJECT, 2020
-- main
-- File description:
-- Main
--

module Main where

import System.IO
import Control.Exception
import System.IO.Error
import Control.Monad
import System.Environment
import System.Exit
import Data.Maybe
import Data.Ix
import Text.Read
import Data.List 
import Lib
import Parse

getColors :: [String] -> Maybe Int
getColors [] = Nothing
getColors args = readMaybe(args !! 0)

getConvergence :: [String] -> Maybe Float
getConvergence [] = Nothing
getConvergence args = readMaybe(args !! 1)

checkArgs:: Maybe Int -> Int
checkArgs (Just x) = x
checkArgs (Nothing) = -1

checkArgsFloat:: Maybe Float -> Float
checkArgsFloat (Just x) = x
checkArgsFloat (Nothing) = -1

checkError :: [String] -> Bool
checkError args
            | (args !! 0) == "-h" = False
            | length args > 3 = True
            | length args < 3 && (args !! 0) /= "-h" = True
            | checkArgs (getColors args) == -1 = True
            | checkArgsFloat (getConvergence args) == -1 = True
            | otherwise = False

main :: IO ()
main = do
    args <- getArgs
    if checkError args then exitWith (ExitFailure 84)
    else Parse.getHelp args (checkArgs (getColors args)) (checkArgsFloat (getConvergence args))
