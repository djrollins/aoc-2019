import           Test.Hspec
import           Lib                            ( fuel
                                                , fuelForFuel
                                                )

main :: IO ()
main = hspec $ do
  describe "fuel" $
    it "calculates the correct amount of fuel" $
      map fuel [12, 14, 1969, 100756] `shouldBe` [2, 2, 654, 33583]
  describe "fuelForFuel" $
    it "calculates how much fuel is required to transport fuel" $
      map fuelForFuel [2, 654, 33583] `shouldBe` [0, 216 + 70 + 21 + 5, 11192 + 3728 + 1240 + 411 + 135 + 43 + 12 + 2]
