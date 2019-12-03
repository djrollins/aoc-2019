module Day03 where

import           System.Environment             ( getArgs )
import Data.List.Split (splitOn)
import Data.Set as S

main :: IO ()
main = do
  (fileName : _) <- getArgs
  (cable1:cable2:_) <- lines <$> readFile fileName
  let coords1 = S.fromList . concatMap (genCoords (0, 0)) . splitOn "," $ cable1
  let coords2 = S.fromList . concatMap (genCoords (0, 0)) . splitOn "," $ cable2
  let intersections = S.intersection coords1 coords2
  print . length $ intersections

genCoords :: (Int, Int) -> String -> [(Int, Int)]
genCoords origin@(x, y) (dir:count) =
  let count' :: Int
      count' = read count
   in Prelude.map move [0..count']
        where move = case dir of
                'U' -> \c -> (x, y + c)
                'D' -> \c -> (x, y - c)
                'L' -> \c -> (x - c, y)
                'R' -> \c -> (x + c, y)

