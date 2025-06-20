module File (updateCRC32, calculateCRC32Content, checkCRC32, calculateCRC32) where

import Data.Binary.Put (putWord32be, runPut)
import qualified Data.ByteString.Lazy as BSL
import STM32CRC
import System.IO (IOMode (WriteMode), withBinaryFile)

-- | Checks if the CRC32 checksum appended to the file is valid.
checkCRC32 :: FilePath -> IO ()
checkCRC32 filePath = do
    fileBS <- BSL.readFile filePath
    let len = BSL.length fileBS
    if len < 4
        then error "File must be at least 4 bytes long"
        else do
            let (mainPart, crcPart) = BSL.splitAt (len - 4) fileBS
            let expectedCRC = runPut $ putWord32be $ crc32Lazy mainPart
            if crcPart == expectedCRC
                then putStrLn "Checksum is valid."
                else putStrLn "Checksum is invalid."

-- | Calculates and prints the CRC32 checksum of a file.
calculateCRC32 :: FilePath -> IO ()
calculateCRC32 filePath = do
    fileBS <- BSL.readFile filePath
    let len = BSL.length fileBS
    if len < 4
        then error "File must be at least 4 bytes long"
        else do
            let (mainPart, _) = BSL.splitAt (len - 4) fileBS
            let crc = crc32Lazy mainPart
            putStrLn $ "CRC32 checksum: " ++ show crc

-- | Updates the CRC32 checksum of a file by recalculating it and appending it to the end.
updateCRC32 :: FilePath -> IO ()
updateCRC32 filePath = do
    fileBS <- BSL.readFile filePath
    let newFileContent = calculateCRC32Content fileBS
    withBinaryFile filePath WriteMode $ \h -> BSL.hPut h newFileContent

-- | Calculates the CRC32 checksum for the content of a file and appends it to the end.
calculateCRC32Content :: BSL.ByteString -> BSL.ByteString
calculateCRC32Content fileBS =
    let len = BSL.length fileBS
     in if len < 4
            then error "File must be at least 4 bytes long"
            else
                let (mainPart, _) = BSL.splitAt (len - 4) fileBS
                    crc = crc32Lazy mainPart
                    crcBytes = runPut $ putWord32be crc
                 in mainPart `BSL.append` crcBytes
