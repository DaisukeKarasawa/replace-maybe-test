module Main (main) where

import System.Environment (getArgs)
import MaybePatterns (Example(..), examples, findExample)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["list"] -> listExamples
    ["all"]  -> runAll
    [exId]    -> runOne exId
    _         -> putStrLn usage

listExamples :: IO ()
listExamples = do
  putStrLn "Available Maybe patterns:"
  mapM_ (\ex -> putStrLn (" - " ++ exampleId ex ++ ": " ++ title ex)) examples

runAll :: IO ()
runAll = mapM_ (\ex -> putStr (prettyExample ex)) examples

runOne :: String -> IO ()
runOne exId = case findExample exId of
  Just ex -> putStr (prettyExample ex)
  Nothing -> putStrLn ("Unknown id: " ++ exId ++ "\n\n" ++ usage)

usage :: String
usage = unlines
  [ "Usage: stack run [list|all|<id>]"
  , "Commands:"
  , "  list          Show available example ids"
  , "  all           Run all examples"
  , "  <id>          Run one example (e.g. basic-fmap)"
  ]

prettyExample :: Example -> String
prettyExample ex =
  let header = "[#] " ++ exampleId ex ++ " - " ++ title ex
      expl   = unlines (map ("  - " ++) (explain ex))
      demo   = "  Demo:\n" ++ indent 4 (runText ex)
      sep    = replicate 40 '-' ++ "\n"
  in unlines [header, expl] ++ demo ++ sep

indent :: Int -> String -> String
indent n s = unlines (map ((replicate n ' ') ++) (lines s))
