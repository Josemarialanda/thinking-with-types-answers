import Test.Hspec

main :: IO ()
main =
  hspec $
    describe "thinking-with-types-answers-test" $
      it "works" $
        2 + 2 `shouldBe` (4 :: Int)
