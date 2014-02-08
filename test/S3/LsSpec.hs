module S3.LsSpec (main, spec) where

import           Control.Monad.Identity
import           S3.Ls
import           System.Exit
import           Test.Hspec

main :: IO ()
main = hspec spec

output :: String
output = unlines [ "2013-12-27 10:34   6346592   s3://fujimuradaisuke/index (1).tar.gz"
                 , "2013-12-27 10:32   6344285   s3://fujimuradaisuke/index.tar.gz"
                 , "2013-10-29 13:34     83864   s3://fujimuradaisuke/otaru.jpg"
                 ]

spec :: Spec
spec = do
    describe "ls" $ do
      it "should return date, size, name of each object" $ do
         let one   = S3Object ("2013-12-27 10:34", "6346592", "fujimuradaisuke/index (1).tar.gz")
             two   = S3Object ("2013-12-27 10:32", "6344285", "fujimuradaisuke/index.tar.gz")
             three = S3Object ("2013-10-29 13:34", "83864",   "fujimuradaisuke/otaru.jpg")

         let res = runIdentity $ ls' (runMock (ExitSuccess, output, "")) "fujimuradaisuke"
         (one `elem` res) `shouldBe` True
