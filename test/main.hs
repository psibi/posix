{-# LANGUAGE OverloadedStrings #-}

import Data.Posix.File.Constant
import qualified Data.Posix.File.IO as IO
import System.Posix.Types (CMode(..))
import Data.Posix.File.Types
import Data.ByteString
import Data.Bits
import Data.Int (Int32)
import Test.Hspec

main :: IO ()
main =
  hspec $
  do readCall
     positionalIO

readCall :: SpecWith ()
readCall = do
  describe "read call" $
    do it "read example" $
         do fd <- IO.open "./test/sampleFile.txt" oRDONLY
            dat <- IO.read fd 30 30
            IO.close fd
            dat `shouldBe` ("This is a sample file.\n", 23)
       it "readMode example" $
         do fd <- IO.openMode "./test/sampleFile.txt" oRDONLY (CMode 0600)
            dat <- IO.read fd 30 30
            IO.close fd
            dat `shouldBe` ("This is a sample file.\n", 23)
       it "write example" $
         do fd <- IO.open "./test/sample.txt" (oCREAT .|. oWRONLY .|. oTRUNC)
            IO.write fd "hello world"
            IO.close fd
            fd <- IO.open "./test/sample.txt" (oRDONLY)
            dat <- IO.read fd 30 30
            IO.close fd
            (fst dat) `shouldBe` "hello world"

positionalIO :: SpecWith ()
positionalIO = do
  describe "Positional I/O" $
    do it "lseek" $
         do fd <- IO.open "./test/sampleFile.txt" oRDONLY
            _ <- IO.lseek fd (OffT 3) (sEEKSET)
            dat <- IO.read fd 30 30
            IO.close fd
            dat `shouldBe` ("s is a sample file.\n", 20)
