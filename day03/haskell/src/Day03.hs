module Day03 where

import           System.Environment             ( getArgs )
import           Data.List.Split                ( splitOn )
import           Data.List                      ( minimumBy
                                                , elemIndex
                                                )
import qualified Data.Set                      as S
import           Data.Maybe                     ( fromJust )
import           Control.Monad.Trans.State

type Coord = (Integer, Integer)

data Direction = U | D | L | R
  deriving Show

data Vector = Vector {
  direction :: Direction,
  count :: Integer
} deriving Show

vector :: String -> Vector
vector (dir : count) = Vector direction (read count)
 where
  direction = case dir of
    'U' -> U
    'D' -> D
    'L' -> L
    'R' -> R


genCoords :: Vector -> State Coord [Coord]
genCoords (Vector dir count) = do
  start <- get
  let coords = scanl move start [1..count]
  put $ last coords
  return $ tail coords
 where
  move = case dir of
      U -> \(x, y) _ -> (x, y + 1)
      D -> \(x, y) _ -> (x, y - 1)
      L -> \(x, y) _ -> (x - 1, y)
      R -> \(x, y) _ -> (x + 1, y)

toCoords :: String -> [Coord]
toCoords cable = 
  let vectors = map vector $ splitOn "," cable
      state = mapM genCoords vectors
   in (0, 0) : (concat . evalState state $ (0, 0))

manhatten :: Coord -> Coord -> Integer
manhatten (p1, p2) (q1, q2) = abs (p1 - q1) + abs (p2 - q2)

nonZero :: Coord -> Bool
nonZero (0, 0) = False
nonZero _      = True

combinedDistance :: [Coord] -> [Coord] -> Coord -> Int
combinedDistance cable1 cable2 to = fromJust $ (+) <$> elemIndex to cable1 <*> elemIndex to cable2

main :: IO ()
main = do
  (fileName : _) <- getArgs
  (cable1   : cable2 : _) <- map toCoords . lines <$> readFile fileName
  let intersections = filter nonZero . S.toList $ S.intersection (S.fromList cable1) (S.fromList cable2)
  let part1 = minimum $ map (manhatten (0, 0)) intersections
  putStrLn $ "part 1: " ++ show part1
  let part2 = minimum $ map (combinedDistance cable1 cable2) intersections
  putStrLn $ "part 2: " ++ show part2
