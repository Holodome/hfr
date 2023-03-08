{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Text
import Options.Applicative

import Cases

data Command
  = CommandReplace
      { old :: Text
      , new :: Text
      , file :: [FilePath]
      }
  | CommandExtChange
      { old :: Text
      , new :: Text
      , files :: [FilePath]
      }
  | CommandCanonicalize
      { canon :: Cases.Case
      , files :: [FilePath]
      }
  deriving (Show)

parseCases :: Parser Cases.Case
parseCases =
  subparser
    (command "pascal" (info (pure Cases.PascalCase) (progDesc "PascalCase")) <>
     command "kebab" (info (pure Cases.KebabCase) (progDesc "kebab-case")) <>
     command "camel" (info (pure Cases.KebabCase) (progDesc "camelCase")) <>
     command
       "skebab"
       (info (pure Cases.KebabCase) (progDesc "SCREAMING-KEBAB-CASE")) <>
     command
       "ssnake"
       (info (pure Cases.KebabCase) (progDesc "SCREAMING_SNAKE_CASE")) <>
     command "snake" (info (pure Cases.KebabCase) (progDesc "snake_case")))

replaceCmd :: Parser Command
replaceCmd =
  CommandReplace <$> argument str (metavar "OLD") <*>
  argument str (metavar "NEW") <*>
  many (argument str (metavar "TARGET..."))

extChangeCmd :: Parser Command
extChangeCmd =
  CommandExtChange <$> argument str (metavar "OLD") <*>
  argument str (metavar "NEW") <*>
  many (argument str (metavar "TARGET..."))

canonCmd :: Parser Command
canonCmd =
  CommandCanonicalize <$> parseCases <*>
  many (argument str (metavar "TARGET..."))

sample :: Parser Command
sample =
  subparser
    (command "rep" (info replaceCmd (progDesc "replace")) <>
     command "ext" (info extChangeCmd (progDesc "change extensions")) <>
     command "canon" (info canonCmd (progDesc "change name canon")))

opts :: ParserInfo Command
opts =
  info
    (sample <**> helper)
    (fullDesc <> progDesc "Filename refactoring tool" <> header "hfr")

main :: IO ()
main = do
  options <- execParser opts
  print options
