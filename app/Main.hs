module Main where

import Lib
import System.Environment (getArgs, getEnv)
import Text.Printf

getPath :: IO [Char]
getPath = do
  home <- getEnv "HOME"
  return $ printf "%s/.ssh/authorized_keys" home

main :: IO ()
main = do
  args <- getArgs
  path <- getPath
  sshManager path args

sshManager :: FilePath -> [String] -> IO ()
sshManager _ [] = putStrLn "You have to provide a command."
sshManager path ["ls"] = listKeys path
sshManager path ["get", id] = getKey path (read id)
sshManager path ("del" : ids) = mapM_ (deleteKey path . read) ids
sshManager path ["add", key, name] = addKey path $ SSHEntry "ssh-rsa" key name
sshManager path ["add", t, key, name] = addKey path $ SSHEntry t key name
sshManager path ["reformat"] = deleteKey path (-1)

sshManager _ (x : _) = printf "Unknown command: %s" x

withKeys :: FilePath -> ([SSHEntry] -> IO ()) -> IO ()
withKeys path f = do
  keys <- keysFromFile path
  f keys

pretty :: (Integer, SSHEntry) -> String
pretty (i, k) = printf "%2d: %s" i (user k)

listKeys :: FilePath -> IO ()
listKeys path = withKeys path $ mapM_ (putStrLn . pretty) . zip [0, 1 ..]

getKey :: FilePath -> Int -> IO ()
getKey path id = withKeys path $ printf "%s" . (!! id)

deleteKey :: FilePath -> Integer -> IO ()
deleteKey path id = withKeys path $ keysToFile path . map snd . filter (\(i, _) -> i /= id) . zip [0 ..]

addKey :: FilePath -> SSHEntry -> IO ()
addKey path key = appendFile path $ printf "\n%s\n" key