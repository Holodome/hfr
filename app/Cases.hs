module Cases where

import Data.Char

data Case
  = KebabCase
  | PascalCase
  deriving (Enum)

split :: Case -> String -> [String]
split KebabCase = splitKebabCase
split PascalCase = split_pascal_case

join :: Case -> [String] -> String
join KebabCase = joinKebabCase
join PascalCase = joinPascalCase

splitKebabCase :: String -> [String]
splitKebabCase = splitOn '-'

joinKebabCase :: [String] -> String
joinKebabCase = joinWith '-'

split_pascal_case :: String -> [String]
split_pascal_case name =
  map
    (map toLower)
    [ take (end - start) (drop start name)
    | (start, end) <- zip idxs $ tail idxs ++ [length name]
    ]
  where
    idxs = upperCaseIdxs name

joinPascalCase :: [String] -> String
joinPascalCase [] = ""
joinPascalCase (car:cdr) = capitalize car ++ joinPascalCase cdr

splitSnakeCase :: String -> [String]
splitSnakeCase = splitOn '_'

joinSnakeCase :: [String] -> String
joinSnakeCase = joinWith '_'

upperCaseIdxs :: String -> [Int]
upperCaseIdxs str =
  map (\(i, _) -> i) $ filter (\(_, c) -> isUpper c) $ enumerate str

enumerate :: [a] -> [(Int, a)]
enumerate it = zip [0 ..] it

capitalize :: String -> String
capitalize it = [toUpper $ head it] ++ tail it

splitOn :: Char -> String -> [String]
splitOn a str =
  words
    [ if c == a
      then ' '
      else c
    | c <- str
    ]

joinWith :: Char -> [String] -> String
joinWith _ [] = ""
joinWith c (car:cdr) = car ++ [c] ++ joinWith c cdr
