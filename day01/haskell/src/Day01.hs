module Day01 where

import           Data.Monoid                    ( Sum(Sum)
                                                , getSum
                                                )
import           Data.Foldable                  ( fold )
import           System.Environment             ( getArgs )
import           Lib                            ( fuel
                                                , fuelForFuel
                                                , totalFuel
                                                )

main :: IO ()
main = do
  (fileName : _) <- getArgs
  modules <- fmap read . lines <$> readFile fileName
  putStrLn $ "part 1: " ++ show (getSum . fold . map (Sum . fuel) $ modules)
  putStrLn $ "part 2: " ++ show (getSum . fold . map (Sum . totalFuel) $ modules)
