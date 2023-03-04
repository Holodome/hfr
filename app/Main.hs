{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Prelude hiding (FilePath)
import Turtle

import Cases

data Command
  = CommandReplace
      { old :: Text
      , new :: Text
      , file :: FilePath
      }
  | CommandExtChange
      { old :: Text
      , new :: Text
      , files :: FilePath
      }
  -- | CommandCanonicalize
  --     { canon :: Cases.Case
  --     , files :: [FilePath]
  --     }
  deriving (Show)

parseCommand :: Parser Command
parseCommand =
  subcommand
    "replace"
    "Replace string in filenames"
    (CommandReplace <$> argText "old" "String to replace" <*>
     argText "new" "String to replace to" <*>
     argPath "path" "Filepath to replace in") <|>
  subcommand
    "ext-change"
    "Change extension"
    (CommandReplace <$> argText "old" "String to replace" <*>
     argText "new" "String to replace to" <*>
     argPath "path" "Filepath to replace in")

main :: IO ()
main = putStrLn "Hello, Haskell!"
