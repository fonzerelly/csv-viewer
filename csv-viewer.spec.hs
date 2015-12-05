import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import qualified CsvViewer.Core as C
import System.Console.Terminal.Size

main::IO ()
main = hspec $ do
   describe "splitIntoRows" $ do
      it "splits csv file into a list of rows" $ do
         C.splitIntoRows "Lorem;Ipsum\ndolor;sit;amet" `shouldBe` [["Lorem", "Ipsum"],["dolor", "sit", "amet"]]

   describe "maxWidth" $ do
      it "returns 0 for empty List" $ do
         C.longest [] `shouldBe` 0

      it "returns 0 for List of empty strings" $ do
         C.longest ["", ""] `shouldBe` 0

      it "calculates the maximum Width of a Column" $ do
         C.longest ["Lorem", "Ipsum", "dolor", "sit"] `shouldBe` 5

   describe "maxLength" $ do
      it "returns the length of the longest row" $ do
         C.longest [["Lorem", "Ipsum"], ["dolor"]] `shouldBe` 2

   describe "centerInRange" $ do
      it "renders a range whitespaces in case of empty string" $ do
         C.centerInRange 10 "" `shouldBe` replicate 10 ' '

      it "renders a range of whitespaces around passed string" $ do
         C.centerInRange 10 "A" `shouldBe` (replicate 4 ' ') ++ "A" ++ (replicate 5 ' ')
         C.centerInRange 10 "AB" `shouldBe` (replicate 4 ' ') ++ "AB" ++ (replicate 4 ' ')

   describe "maximizeRows" $ do
      it "ensures each Row has same length" $ do
         C.maximizeRows [["Lorem", "Ipsum"], ["sit"]] `shouldBe` [["Lorem", "Ipsum"],["sit", ""]]

   describe "maximizeColumns" $ do
      it "passes empty Array with an empty array" $ do
         C.maximizeColumns [] `shouldBe` []

      it "passes an array of empty arrays as such" $ do
         C.maximizeColumns [[],[]] `shouldBe` [[],[]]

      it "wraps each cell in the colum but the largest in whitespaces" $ do
         let columns = [["Lorem", "dolo", "sit"], ["LoremIpsum", "dolo", "sit"]]
             result = [["Lorem", "dolo ", " sit "], ["LoremIpsum", "   dolo   ", "   sit    "]]
         C.maximizeColumns columns `shouldBe` result

      it "fills also empty columns with whitespaces" $ do
         let columns = [["", "Lorem"], ["dolor", "sit"]]
             result = [["     ","Lorem"], ["dolor", " sit "]]
         C.maximizeColumns columns `shouldBe` result


   describe "transformTable" $ do
      it "maximizesColumns on each row" $ do
         let rows = [["Lorem", "dolo", "sitdipate"], ["LoremIpsum", "dolo", "sit"]]
             result = [["  Lorem   ", "dolo", "sitdipate"], ["LoremIpsum", "dolo", "   sit   "]]
         C.transformTable rows `shouldBe` result

      it "fills also empty columns with whitespaces" $ do
         let columns = [["Lorem", "dolor"], ["sit"]]
             result = [["Lorem", "dolor"], [" sit ", "     "]]
         C.transformTable columns `shouldBe` result

   describe "joinRowsByPipe" $ do
      it "renders string out of [Rows] where each colum is sparated by a pipe" $ do
         let rows = [["Lorem", "Ipsum"], ["dolor", "sit"]]
             result = "Lorem|Ipsum\ndolor|sit\n"
         C.joinRowsByPipe rows `shouldBe` result

   describe "displayCsv" $ do
      it "renders pipes between equal sized columns" $ do
         C.displayCsv "Lorem;dolor;sit\nAmend;ganimet" `shouldBe` "Lorem| dolor |sit\nAmend|ganimet|   \n"

   describe "slice" $ do
      it "slices rows out of table" $ do
         let rows = [["Lorem", "Ipsum"], ["dolor", "sit"], ["amend", "paralu"]]
         C.slice 0 100 rows `shouldBe` rows
         C.slice 1 100 rows `shouldBe` [["dolor", "sit"], ["amend", "paralu"]]
         C.slice 1 1 rows `shouldBe` [["dolor", "sit"]]

      it "slices colums out of row" $ do
         let row = ["Lorem", "Ipsum", "dolor"]
         C.slice 0 100 row `shouldBe` row
         C.slice 1 100 row `shouldBe` ["Ipsum", "dolor"]
         C.slice 1 1 row `shouldBe` ["Ipsum"]


   describe "truncateTable" $ do
      let appliedTruncateTable = C.truncateTable $ C.splitIntoRows " abc ;abcdefg;ab\n12345; 12345 ;12\n     ;zyxwvut;zy"
      it "should drop all cells above cell offset" $ do
         appliedTruncateTable (1,1) (Window 100 100) `shouldBe` " 12345 |12\nzyxwvut|zy\n"
         appliedTruncateTable (0,2) (Window 100 100) `shouldBe` "     |zyxwvut|zy\n"

      it "should take only stringified cells inside window" $ do
         appliedTruncateTable (0,0) (Window 2 10) `shouldBe` " abc |abcd\n12345| 123\n"

      it "should take only stringified cells inside window beyond cell offset" $ do
         appliedTruncateTable (1,1) (Window 1 8) `shouldBe` " 12345 |\n"

   describe "resize" $ do
      let win = Window 10 10
          appliedResize = C.resize win

      it "should create a new window based on the original window" $ do
         appliedResize 0 0 `shouldBe` win

      it "should create a window resized by hight" $ do
         appliedResize 1 0 `shouldBe` Window 11 10

      it "should create a window resized by width" $ do
         appliedResize 0 (-1) `shouldBe` Window 10 9
