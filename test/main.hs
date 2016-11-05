{-# LANGUAGE OverloadedStrings #-}

import Data.Posix.File.Constant
import qualified Data.Posix.File.IO as IO
import System.Posix.Types (CMode(..))
import Data.ByteString
import Data.Bits
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
          it "readMode example" $
            do fd <- IO.openMode "./test/sampleFile.txt" o_rdonly (CMode 0600)
               dat <- IO.read fd 30 30
               IO.close fd
               dat `shouldBe` ("This is a sample file.\n", 23)
          it "write example" $
            do fd <- IO.open "./test/sample.txt" (o_wronly .|. o_trunc)
               IO.write fd "hello world"
               IO.close fd
               fd <- IO.open "./test/sample.txt" (o_rdonly)
               dat <- IO.read fd 30 30
               IO.close fd
               (fst dat) `shouldBe` "hello world"
