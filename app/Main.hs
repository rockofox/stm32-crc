module Main (main) where

import Options.Applicative
import File
import Numeric (showHex)
import qualified Data.ByteString.Lazy.Char8 as BSL

main :: IO ()
main = execParser opts >>= runCommand

opts :: ParserInfo Command
opts = info (commands <**> helper)
    ( fullDesc
   <> progDesc "STM32 CRC utility"
   <> header "stm32-crc - A utility for working with STM32 CRC32 checksums" )

data Command = Update FilePath | Check FilePath | Calculate FilePath

commands :: Parser Command
commands = subparser
    ( command "update" (info (Update <$> argument str (metavar "FILE")) (progDesc "Update CRC checksum in file"))
   <> command "check" (info (Check <$> argument str (metavar "FILE")) (progDesc "Check if CRC checksum is valid"))
   <> command "calculate" (info (Calculate <$> argument str (metavar "FILE")) (progDesc "Calculate CRC checksum and output to stdout")) )

runCommand :: Command -> IO ()
runCommand (Update filePath) =
    if filePath == "-" then getContents >>= updateCRC32Content . BSL.pack else updateCRC32 filePath
runCommand (Check filePath) =
    if filePath == "-" then getContents >>= checkCRC32Content . BSL.pack else checkCRC32 filePath
runCommand (Calculate filePath) =
    if filePath == "-" then getContents >>= \content -> putStrLn $ Numeric.showHex (extractChecksum $ BSL.pack content) "" else calculateCRC32 filePath

