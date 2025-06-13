module STM32CRC (crc32, crc32Lazy) where

import Control.Monad (replicateM)
import Data.Array.Unboxed
import Data.Binary.Get (Get, getWord8, runGet)
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Lazy as L
import Data.List (foldl')
import Data.Word

-- | CRC32 lookup table (identical to C implementation)
crcTable :: UArray Word32 Word32
crcTable = listArray (0, 255) $ map mkEntry [0 .. 255]
 where
  mkEntry :: Word32 -> Word32
  mkEntry n = go (n `shiftL` 24) 8
   where
    poly = 0x04C11DB7
    go :: Word32 -> Int -> Word32
    go crc 0 = crc
    go crc i =
      let flag = testBit crc 31
          crc' = crc `shiftL` 1
          crc'' = if flag then crc' `xor` poly else crc'
       in go crc'' (i - 1)

-- | Process a single Word32 through the CRC table 4 times
processWord :: Word32 -> Word32
processWord = go 4
 where
  go :: Int -> Word32 -> Word32
  go 0 crc = crc
  go n crc =
    let idx = fromIntegral $ (crc `shiftR` 24) .&. 0xFF
        next = (crc `shiftL` 8) `xor` (crcTable ! idx)
     in go (n - 1) next

-- | Process 4-byte chunk
updateCRC :: Word32 -> Word32 -> Word32
updateCRC crc w = processWord (crc `xor` w)

-- | Pad tail bytes into 32-bit value (matching C implementation)
padTail :: [Word8] -> Word32
padTail [b0] = fromIntegral b0 `shiftL` 24
padTail [b0, b1] = (fromIntegral b0 .|. (fromIntegral b1 `shiftL` 8)) `shiftL` 16
padTail [b0, b1, b2] =
  (fromIntegral b0 `shiftL` 8)
    .|. (fromIntegral b1 `shiftL` 16)
    .|. (fromIntegral b2 `shiftL` 24)
padTail _ = 0xFFFFFFFF

-- | Convert ByteString to 32-bit chunks and tail bytes
splitChunks :: B.ByteString -> ([Word32], [Word8])
splitChunks input = (words, B.unpack tail)
 where
  len = B.length input
  chunkLen = len - (len `mod` 4)
  (chunks, tail) = B.splitAt chunkLen input
  words = runGet (replicateM (chunkLen `div` 4) getWord32le) (L.fromStrict chunks)

-- | Little-endian Word32 parser
getWord32le :: Get Word32
getWord32le = do
  b0 <- fromIntegral <$> getWord8
  b1 <- fromIntegral <$> getWord8
  b2 <- fromIntegral <$> getWord8
  b3 <- fromIntegral <$> getWord8
  return $ b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24)

-- | Core CRC32 calculationcalculateCRC :: B.ByteString -> Word32
calculateCRC input = finalCRC
 where
  (words, tailBytes) = splitChunks input
  initial = 0xFFFFFFFF
  processed = foldl' updateCRC initial words
  finalCRC =
    if null tailBytes
      then processed
      else updateCRC processed (padTail tailBytes)

-- | Convert little endian to big endian
le2be :: Word32 -> Word32
le2be w =
  ((w `shiftR` 24) .&. 0xFF)
    .|. ((w `shiftR` 8) .&. 0xFF00)
    .|. ((w `shiftL` 8) .&. 0xFF0000)
    .|. ((w `shiftL` 24) .&. 0xFF000000)

-- | Main CRC32 interface for strict ByteStrings
crc32 :: B.ByteString -> Word32
crc32 = le2be . calculateCRC

-- | Lazy ByteString version (for large files)
crc32Lazy :: L.ByteString -> Word32
crc32Lazy = le2be . calculateCRC . B.concat . L.toChunks
