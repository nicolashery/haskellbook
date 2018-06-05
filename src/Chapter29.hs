module Chapter29 where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hGetLine, hPutStrLn, hWaitForInput, stderr, stdin, stdout)

import Cipher (unVigenere, vigenere)

-- File I/O with Vigenère

-- Can test this in GHCI with:
--   λ> import System.Environment (withArgs)
--   λ> withArgs ["penguin", "-e"] mainVigenere

data Mode
  = Encode
  | Decode
  deriving (Eq, Show)

mainVigenere :: IO ()
mainVigenere = do
  args <- getArgs
  case args of
    (keyword:modeFlag:_) -> do
      let maybeMode = parseModeFlag modeFlag
      case maybeMode of
        Just mode -> do
          gotInput <- hWaitForInput stdin 10000
          if gotInput then do
            message <- hGetLine stdin
            handleInput keyword mode message
          else do
            hPutStrLn stderr $ "Timeout exceeded"
            exitFailure
        Nothing -> do
          hPutStrLn stderr $ "Unrecognized mode '" ++ modeFlag ++ "', must be '-e' or '-d'"
          exitFailure
    _ -> do
      hPutStrLn stderr "Must provide a secret keyword and a mode ('-e', '-d') as arguments"
      exitFailure

parseModeFlag :: String -> Maybe Mode
parseModeFlag s =
  case s of
    "-e" -> Just Encode
    "-d" -> Just Decode
    _ -> Nothing

handleInput :: String -> Mode -> String -> IO ()
handleInput keyword mode message =
  case mode of
    Encode -> hPutStrLn stdout (vigenere keyword message)
    Decode -> hPutStrLn stdout (unVigenere keyword message)

-- TODO: Config directories
