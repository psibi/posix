{-# LANGUAGE OverloadedStrings #-}

import Data.Posix.File.Constant
import qualified Data.Posix.File.IO as IO
import System.Posix.Types (CMode(..), Fd(..))
import Data.Posix.Internal.FdSet
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
     fdSetTypeTest

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
            _ <- IO.lseek fd 3 (sEEKSET)
            dat <- IO.read fd 30 30
            IO.close fd
            dat `shouldBe` ("s is a sample file.\n", 20)
       it "pread" $
         do fd <- IO.open "./test/sampleFile.txt" oRDONLY
            dat <- IO.pread fd 30 0
            IO.close fd
            dat `shouldBe` ("This is a sample file.\n", 23)
       it "pwrite" $
         do fd <- IO.open "./test/sample.txt" (oCREAT .|. oWRONLY .|. oTRUNC)
            IO.pwrite fd "hello world" 0
            IO.close fd
            fd <- IO.open "./test/sample.txt" (oRDONLY)
            dat <- IO.read fd 30 30
            IO.close fd
            (fst dat) `shouldBe` "hello world"

fdSetTypeTest :: SpecWith ()
fdSetTypeTest = do
  describe "fdset macros test" $
    do it "fd_set macro" $
         do fdset <- emptyFdSet
            fdSet (Fd 5) fdset
            ret <- fdIsSet (Fd 5) fdset
            ret `shouldNotBe` 0
       it "fd_clr macro" $
         do fdset <- emptyFdSet
            fdSet (Fd 5) fdset
            fdClr (Fd 5) fdset
            ret <- fdIsSet (Fd 5) fdset
            ret `shouldBe` 0
       it "fd_zero macro" $
         do fdset <- emptyFdSet
            fdSet (Fd 5) fdset
            fdZero fdset
            ret <- fdIsSet (Fd 5) fdset
            ret `shouldBe` 0
