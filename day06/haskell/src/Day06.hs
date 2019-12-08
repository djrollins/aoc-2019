module Day06 where

import qualified Data.Map                      as M
import Data.Maybe (fromJust, isNothing, isJust)
import           Data.List.Split                ( splitOn )
import           System.Environment             ( getArgs )
import Control.Monad (join)

type System = M.Map String [String]

merge :: System -> String -> System
merge map input = M.insertWith (++) planet [orbitor] map
 where
  [planet,orbitor] = splitOn ")" input

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

checksum :: System -> Int
checksum planets = checksum' 0 "COM"
 where
  checksum' acc planet =
    acc + (sum . map (checksum' (acc + 1)) . M.findWithDefault [] planet $ planets)

pathTo :: String -> System -> [String]
pathTo person system = reverse $ fromJust $ findFrom [] "COM"
 where
  findFrom :: [String] -> String -> Maybe [String]
  findFrom path planet
    | person `elem` orbitors = Just $ planet:path
    | null orbitors = Nothing
    | otherwise =  join . safeHead . filter (not . null) . map (findFrom (planet:path)) $ orbitors
        where orbitors = M.findWithDefault [] planet system

pathBetween :: String -> String -> System -> Int
pathBetween start end system =
  let toStart = pathTo start system
      toEnd = pathTo end system
   in length . uncurry (++) $ dropCommonPrefix toStart toEnd
      where dropCommonPrefix :: [String] -> [String] -> ([String], [String])
            dropCommonPrefix (f:fs) (s:ss) = if f /= s then (f:fs, s:ss) else dropCommonPrefix fs ss

main :: IO ()
main = do
  (fileName : _) <- getArgs
  input          <- lines <$> readFile fileName
  let planets = foldl merge M.empty input
  putStrLn $ "part 1: " ++ show (checksum planets)
  putStrLn $ "part 2: " ++ show (pathBetween "YOU" "SAN" planets)
