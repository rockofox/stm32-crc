{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (evaluate)
import Data.Binary.Put (putWord32be, runPut)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy as L
import STM32CRC (crc32, crc32Lazy)
import System.Environment (withArgs)
import System.IO (IOMode (ReadMode), hClose, hGetContents, withBinaryFile)
import System.IO.Temp (withSystemTempFile)
import Test.Hspec
import File (calculateCRC32Content, updateCRC32)

main :: IO ()
main = hspec $ do
    describe "calculateCRC32Content" $ do
        it "appends CRC32 to ByteString longer than 4 bytes" $ do
            let original = BSL.pack [1, 2, 3, 4, 5, 6, 7, 8]
                result = calculateCRC32Content original
                (mainPart, crcPart) = BSL.splitAt (BSL.length original - 4) result
            mainPart `shouldBe` BSL.take (BSL.length original - 4) original
            BSL.length result `shouldBe` BSL.length original
        it "throws error for ByteString shorter than 4 bytes" $ do
            evaluate (calculateCRC32Content $ BSL.pack [1, 2, 3]) `shouldThrow` anyErrorCall
        it "produces correct CRC32 for known data" $ do
            let original = BSL.pack [1, 2, 3, 4, 5, 6, 7, 8]
                result = calculateCRC32Content original
                crcBytes = BSL.drop (BSL.length original - 4) result
            crcBytes `shouldBe` "Oç«\GS"

    describe "updateCRC32" $ do
        it "updates the CRC32 checksum in a simulated file" $ do
            let original = BSL.pack [1, 2, 3, 4, 5, 6, 7, 8, 0, 0, 0, 0]
                updated = calculateCRC32Content original
            updated `shouldNotBe` original
            let (mainPart, crcPart) = BSL.splitAt (BSL.length updated - 4) updated
            mainPart `shouldBe` BSL.take (BSL.length original - 4) original

    describe "checkCRC32" $ do
        it "validates the CRC32 checksum for a simulated file" $ do
            let original = BSL.pack [1, 2, 3, 4, 5, 6, 7, 8]
                updated = calculateCRC32Content original
            let (mainPart, crcPart) = BSL.splitAt (BSL.length updated - 4) updated
            let expectedCRC = runPut $ putWord32be $ crc32Lazy mainPart
            crcPart `shouldBe` expectedCRC

    describe "calculateCRC32" $ do
        it "calculates and prints the CRC32 checksum for known data" $ do
            let original = BSL.pack [1, 2, 3, 4, 5, 6, 7, 8]
                calculatedCRC = crc32Lazy original
                expectedCRC = 3659207843
            calculatedCRC `shouldBe` expectedCRC

        describe "STM32CRC" $ do
            it "calculates crc32 correctly for strict ByteString" $ do
                let input = "123456789" :: B.ByteString
                crc32 input `shouldBe` 660893793

            it "calculates crc32Lazy correctly for lazy ByteString" $ do
                let input = L.fromStrict "123456789"
                crc32Lazy input `shouldBe` 660893793
