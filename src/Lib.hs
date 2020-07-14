module Lib
    ( euclidianRange
    , getAllEuclidian
    , getAllRand
    , findShortest
    , maxIndex
    , calcMinDistance
    , getClusters
    , getAllClusters
    , extractFstTuple
    , createAverageTuple
    , getAllAverage
    , stopCondition
    , kMeansLoop
    , printAllFindShortest
    , printGetAllEuclidians
    ) where

import System.Random
import Text.Printf

euclidianRange :: (Int, Int, Int) -> (Int, Int, Int) -> Float
euclidianRange (x1, y1, z1) (x2, y2, z2) = sqrt ((fromIntegral (x1 - x2) ^ 2) + (fromIntegral (y1 - y2) ^ 2) + (fromIntegral (z1 - z2) ^ 2))
deleteN list n = mappend (take n list) (drop (n + 1) list)

getAllRand :: Int -> [((Int, Int), (Int, Int, Int))] -> [((Int, Int), (Int, Int, Int))] -> StdGen -> [((Int, Int), (Int, Int, Int))]
getAllRand 0 _ res _ = res
getAllRand nb list res gen = do
                            let (i1, s2) = randomR (0, ((length list) - 1):: Int) gen
                            getAllRand (nb - 1) (deleteN list i1) (res ++ [list !! i1]) gen

getAllEuclidian :: [((Int, Int), (Int, Int, Int))] -> ((Int, Int), (Int, Int, Int)) -> [Float] -> [Float]
getAllEuclidian [] centroid res = res
getAllEuclidian (x:xs) centroid res = getAllEuclidian xs centroid (res++[euclidianRange (snd x) (snd centroid)])

maxIndex xs = head $ filter ((== minimum xs) . (xs !!)) [0..]

findShortest :: [((Int, Int), (Int, Int, Int))] -> ((Int, Int), (Int, Int, Int)) -> ((Int, Int), (Int, Int, Int))
findShortest randTab value = randTab !! (maxIndex $ getAllEuclidian randTab value [])

printAllFindShortest :: [((Int, Int), (Int, Int, Int))] -> [((Int, Int), (Int, Int, Int))] -> IO()
printAllFindShortest randTab [] = putStrLn ""
printAllFindShortest randTab (x:xs) = do 
                                    print $ findShortest randTab x
                                    printAllFindShortest randTab xs  

printGetAllEuclidians :: [((Int, Int), (Int, Int, Int))] -> [((Int, Int), (Int, Int, Int))] -> IO ()
printGetAllEuclidians randTab [] = putStrLn ""
printGetAllEuclidians randTab (x:xs) = do
                                    print $ getAllEuclidian randTab x []
                                    printGetAllEuclidians randTab xs 

calcMinDistance :: [((Int, Int), (Int, Int, Int))] -> [((Int, Int), (Int, Int, Int))] -> Int -> [(((Int, Int), (Int, Int, Int)), Int)] -> [(((Int, Int), (Int, Int, Int)), Int)]
calcMinDistance [] randTab index res = res
calcMinDistance (x:xs) randTab index res = calcMinDistance xs randTab (index + 1) (res ++[(findShortest randTab x, index)])

getClusters :: [(((Int, Int), (Int, Int, Int)), Int)] -> ((Int, Int), (Int, Int, Int)) -> [((Int, Int), (Int, Int, Int))] -> [((Int, Int), (Int, Int, Int))] -> [((Int, Int), (Int, Int, Int))]
getClusters [] centroid values res = res
getClusters (x:xs) centroid values res
                                | (snd (fst x)) == (snd centroid) = getClusters xs centroid values (res ++ [values !! (snd x)])
                                | otherwise = getClusters xs centroid values res

getAllClusters :: [(((Int, Int), (Int, Int, Int)), Int)] -> [((Int, Int), (Int, Int, Int))] -> [((Int, Int), (Int, Int, Int))]  -> [[((Int, Int), (Int, Int, Int))]] -> [[((Int, Int), (Int, Int, Int))]]
getAllClusters assosTab [] values res = res
getAllClusters assosTab (x:xs) values res = do
                                            let temp = getClusters assosTab x values []
                                            getAllClusters assosTab xs values (res++[temp])

getFst :: (Int, Int, Int) -> Int
getFst (a, b, c) = a

extractFstTuple :: [((Int, Int), (Int, Int, Int))] -> [Int] -> [Int]
extractFstTuple [] res = res
extractFstTuple (x:xs) res = extractFstTuple xs (res ++ [getFst (snd x)])

getSnd :: (Int, Int, Int) -> Int
getSnd (a, b, c) = b

extractSndTuple :: [((Int, Int), (Int, Int, Int))] -> [Int] -> [Int]
extractSndTuple [] res = res
extractSndTuple (x:xs) res = extractSndTuple xs (res ++ [getSnd (snd x)])

getThd :: (Int, Int, Int) -> Int
getThd (a, b, c) = c

extractThdTuple :: [((Int, Int), (Int, Int, Int))] -> [Int] -> [Int]
extractThdTuple [] res = res
extractThdTuple (x:xs) res = extractThdTuple xs (res ++ [getThd (snd x)])

extractFstPoint :: [((Int, Int), (Int, Int, Int))] -> [Int] -> [Int]
extractFstPoint [] res = res
extractFstPoint (x:xs) res = extractFstPoint xs (res ++ [fst (fst x)])

extractSndPoint :: [((Int, Int), (Int, Int, Int))] -> [Int] -> [Int]
extractSndPoint [] res = res
extractSndPoint (x:xs) res = extractSndPoint xs (res ++ [snd (fst x)])

average :: [Int] -> Int
average xs = (sum xs) `div` (length xs)

createAverageTuple :: [((Int, Int), (Int, Int, Int))] -> ((Int, Int), (Int, Int, Int))
createAverageTuple tupList = ((average $ extractFstPoint tupList [], average $ extractSndPoint tupList []), (average $ extractFstTuple tupList [], average $ extractSndTuple tupList [], average $ extractThdTuple tupList []))

getAllAverage :: [[((Int, Int), (Int, Int, Int))]] -> [((Int, Int), (Int, Int, Int))] -> [((Int, Int), (Int, Int, Int))]
getAllAverage  [] res = res
getAllAverage (x:xs) res = getAllAverage xs (res ++ [createAverageTuple x])

stopCondition :: [((Int, Int), (Int, Int, Int))] -> [((Int, Int), (Int, Int, Int))] -> Float-> Bool
stopCondition [] [] convergence = False
stopCondition (a:as) (b:bs) convergence
                            | euclidianRange (snd $ a) (snd $ b) <= convergence = True
                            | otherwise = stopCondition as bs convergence

printWeird :: [((Int, Int), (Int, Int, Int))] -> IO ()
printWeird [] = putStr ""
printWeird (x:xs) = do 
                    printf "(%d,%d) (%d,%d,%d)\n" (fst(fst x)) (snd(fst x)) (getFst(snd x)) (getSnd(snd x)) (getThd (snd x))
                    printWeird xs
printAll :: [((Int, Int), (Int, Int, Int))] -> [[((Int, Int), (Int, Int, Int))]] -> IO ()
printAll [] [] = putStr ""
printAll (x:xs) (y:ys) = do
                        putStrLn "--"
                        print $ snd x
                        putStrLn "-"
                        printWeird y
                        printAll xs ys
kMeansLoop :: [((Int, Int), (Int, Int, Int))] -> [((Int, Int), (Int, Int, Int))]-> [((Int, Int), (Int, Int, Int))] -> Float -> IO()
kMeansLoop randTab averTab coords conv
                            | stopCondition randTab averTab conv = do
                                printAll averTab (getAllClusters (calcMinDistance coords randTab 0 []) randTab coords [])
                            | otherwise = kMeansLoop averTab (getAllAverage (getAllClusters (calcMinDistance coords randTab 0 []) randTab coords []) []) coords conv