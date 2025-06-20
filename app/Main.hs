module Main (main) where

import Options.Applicative
import File

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
runCommand (Update filePath) = updateCRC32 filePath
runCommand (Check filePath) = checkCRC32 filePath
runCommand (Calculate filePath) = calculateCRC32 filePath

