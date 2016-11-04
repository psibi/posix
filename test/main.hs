{-# LANGUAGE OverloadedStrings #-}

import Data.C.File.Constant
import qualified Data.C.File.IO as IO
import Data.ByteString
import Test.Hspec

main :: IO ()
main =
  hspec $
  do describe "read call" $
       do it "read example" $
            do fd <- IO.open "./test/sampleFile.txt" o_rdonly
               dat <- IO.read fd 30 30
               IO.close fd
               dat `shouldBe` ("This is a sample file.\n", 23)
