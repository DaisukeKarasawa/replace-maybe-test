module Main (main) where

import System.Environment (getArgs, lookupEnv)
import Data.Maybe (listToMaybe, fromMaybe, mapMaybe)
import Data.List (isPrefixOf)
import Control.Applicative ((<|>))
import Control.Monad (mfilter)
import Text.Read (readMaybe)
import Data.Char (toUpper, toLower)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("greet":rest)    -> cmdGreet rest
    ("sum":rest)      -> cmdSum rest
    ("head":fp:_)     -> cmdHead fp
    ("email":name:_)  -> cmdEmail name
    _                  -> putStrLn usage

usage :: String
usage = unlines
  [ "Usage: stack run -- <command> [args]"
  , ""
  , "Commands:"
  , "  greet [--yell] [<name>]     Print a greeting (fallback to $USER or 'stranger')"
  , "  sum [--strict|--lenient] <nums...>  Sum integers (strict fails on invalid tokens)"
  , "  head <file>                 Print first line of a file (or 'Empty file')"
  , "  email <name>                Lookup email by name"
  ]

cmdGreet :: [String] -> IO ()
cmdGreet args = do
  envUser <- lookupEnv "USER"
  let isFlag s = "--" `isPrefixOf` s
      yell     = any (== "--yell") args
      nameArg  = listToMaybe [a | a <- args, not (isFlag a)]
      chosen   = fromMaybe "stranger"
               $ mfilter (not . null) nameArg
             <|> mfilter (not . null) envUser
      out      = (if yell then map toUpper else id) ("Hello, " ++ chosen ++ "!")
  putStrLn out

cmdSum :: [String] -> IO ()
cmdSum args = do
  let isFlag s = "--" `isPrefixOf` s
      strict   = any (== "--strict") args
      tokens   = [a | a <- args, not (isFlag a)]
  if strict
    then case traverse (readMaybe :: String -> Maybe Int) tokens of
      Just xs -> print (sum xs)
      Nothing ->
        let invalids = [t | t <- tokens, (readMaybe t :: Maybe Int) == Nothing]
        in putStrLn $ case listToMaybe invalids of
             Just bad -> "Error: invalid number: " ++ bad
             Nothing  -> "Error: invalid input"
    else do
      let xs = mapMaybe (readMaybe :: String -> Maybe Int) tokens
      print (sum xs)

cmdHead :: FilePath -> IO ()
cmdHead fp = do
  content <- readFile fp
  let mFirst = listToMaybe (lines content)
  putStrLn (maybe "Empty file" id mFirst)

cmdEmail :: String -> IO ()
cmdEmail name = do
  let userIdByName      = [("alice", 1 :: Int), ("bob", 2)]
      userIdByNameLower = [(map toLower k, v) | (k, v) <- userIdByName]
      emailById         = [(1, "alice@example.com"), (2, "bob@example.com")]
      uid               = lookup name userIdByName
                       <|> lookup (map toLower name) userIdByNameLower
      email             = uid >>= (\u -> lookup u emailById)
  putStrLn (fromMaybe "not found" email)
