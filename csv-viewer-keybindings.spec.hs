import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import CsvViewer.Keybindings

main::IO ()
main = hspec $ do
   describe "convertKey" $ do
      it "Converts getch-codes into proprietery types" $ do
         convertKey 72 `shouldBe` UP
         convertKey 80 `shouldBe` DOWN
         convertKey 3  `shouldBe` QUIT
         convertKey 4  `shouldBe` QUIT
         convertKey 27 `shouldBe` QUIT
