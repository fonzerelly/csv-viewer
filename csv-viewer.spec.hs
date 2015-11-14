import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import CsvViewer.Core

main::IO ()
main = hspec $ do
   describe "splitIntoRows" $ do
      it "splits csv file into a list of rows" $ do
         splitIntoRows "Lorem;Ipsum\ndolor;sit;amet" `shouldBe` [["Lorem", "Ipsum"],["dolor", "sit", "amet"]]

   describe "maxWidth" $ do
      it "returns 0 for empty List" $ do
         longest [] `shouldBe` 0

      it "returns 0 for List of empty strings" $ do
         longest ["", ""] `shouldBe` 0

      it "calculates the maximum Width of a Column" $ do
         longest ["Lorem", "Ipsum", "dolor", "sit"] `shouldBe` 5

   describe "maxLength" $ do
      it "returns the length of the longest row" $ do
         longest [["Lorem", "Ipsum"], ["dolor"]] `shouldBe` 2

   describe "centerInRange" $ do
      it "renders a range whitespaces in case of empty string" $ do
         centerInRange 10 "" `shouldBe` replicate 10 ' '

      it "renders a range of whitespaces around passed string" $ do
         centerInRange 10 "A" `shouldBe` (replicate 4 ' ') ++ "A" ++ (replicate 5 ' ')
         centerInRange 10 "AB" `shouldBe` (replicate 4 ' ') ++ "AB" ++ (replicate 4 ' ')

   describe "maximizeRows" $ do
      it "ensures each Row has same length" $ do
         maximizeRows [["Lorem", "Ipsum"], ["sit"]] `shouldBe` [["Lorem", "Ipsum"],["sit", ""]]

   describe "maximizeColumns" $ do
      it "passes empty Array with an empty array" $ do
         maximizeColumns [] `shouldBe` []

      it "passes an array of empty arrays as such" $ do
         maximizeColumns [[],[]] `shouldBe` [[],[]]

      it "wraps each cell in the colum but the largest in whitespaces" $ do
         let columns = [["Lorem", "dolo", "sit"], ["LoremIpsum", "dolo", "sit"]]
             result = [["Lorem", "dolo ", " sit "], ["LoremIpsum", "   dolo   ", "   sit    "]]
         maximizeColumns columns `shouldBe` result

      it "fills also empty columns with whitespaces" $ do
         let columns = [["", "Lorem"], ["dolor", "sit"]]
             result = [["     ","Lorem"], ["dolor", " sit "]]
         maximizeColumns columns `shouldBe` result


   describe "transformTable" $ do
      it "maximizesColumns on each row" $ do
         let rows = [["Lorem", "dolo", "sitdipate"], ["LoremIpsum", "dolo", "sit"]]
             result = [["  Lorem   ", "dolo", "sitdipate"], ["LoremIpsum", "dolo", "   sit   "]]
         transformTable rows `shouldBe` result

      it "fills also empty columns with whitespaces" $ do
         let columns = [["Lorem", "dolor"], ["sit"]]
             result = [["Lorem", "dolor"], [" sit ", "     "]]
         transformTable columns `shouldBe` result

   describe "joinRowsByPipe" $ do
      it "renders string out of [Rows] where each colum is sparated by a pipe" $ do
         let rows = [["Lorem", "Ipsum"], ["dolor", "sit"]]
             result = "Lorem|Ipsum\ndolor|sit\n"
         joinRowsByPipe rows `shouldBe` result

   describe "displayCsv" $ do
      it "renders pipes between equal sized columns" $ do
         displayCsv "Lorem;dolor;sit\nAmend;ganimet" `shouldBe` "Lorem| dolor |sit\nAmend|ganimet|   \n"

