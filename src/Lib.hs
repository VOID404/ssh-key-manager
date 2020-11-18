{-# LANGUAGE BangPatterns #-}

module Lib
  ( keysFromFile,
    keysToFile,
    SSHEntry (..),
  )
where

import Data.Char
import System.Directory
import Text.Printf

data SSHEntry = SSHEntry
  { key :: String,
    user :: String
  }
  deriving (Show)

instance PrintfArg SSHEntry where
  formatArg k fmt =
    formatString (unwords ["ssh-rsa", key k, user k]) (fmt {fmtChar = 's', fmtPrecision = Nothing})

keysFromFile :: FilePath -> IO [SSHEntry]
keysFromFile p = do
  contents <- readFile p
  return . map lineToSSHEntry . filter (not . all isSpace) $ lines contents

keysToFile :: FilePath -> [SSHEntry] -> IO ()
keysToFile p l = do
  let !_ = length l
  let old = printf "%s.bak" p
  renameFile p old
  writeFile p ""
  mapM_ (appendFile p . printf "%s\n") l

lineToSSHEntry :: String -> SSHEntry
lineToSSHEntry l = SSHEntry (words l !! 1) (unwords . drop 2 . words $ l)