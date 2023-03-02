module Cases where

import Data.Char

data Case
  = KebabCase
  | PascalCase
  | SnakeCase
  | CamelCase
  | ScreamingSnakeCase
  | ScreamingKebabCase
  deriving (Enum)

split :: Case -> String -> [String]
split KebabCase = splitOn '-'
split PascalCase = splitPascalCase
split SnakeCase = splitOn '_'
split CamelCase = splitCamelCase
split ScreamingSnakeCase = split SnakeCase . map toLower
split ScreamingKebabCase = split KebabCase . map toLower

join :: Case -> [String] -> String
join KebabCase = joinWith '-'
join PascalCase = joinPascalCase
join SnakeCase = joinWith '_'
join CamelCase = joinCamelCase
join ScreamingSnakeCase = map toUpper . join SnakeCase
join ScreamingKebabCase = map toUpper . join KebabCase

splitPascalCase :: String -> [String]
splitPascalCase name = splitByIdxs (upperCaseIdxs name) name

joinPascalCase :: [String] -> String
joinPascalCase [] = ""
joinPascalCase (car:cdr) = capitalize car ++ joinPascalCase cdr

splitCamelCase :: String -> [String]
splitCamelCase name = splitByIdxs ([0] ++ upperCaseIdxs name) name

joinCamelCase :: [String] -> String
joinCamelCase [] = ""
joinCamelCase (car:cdr) = car ++ joinPascalCase cdr

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

splitByIdxs :: [Int] -> String -> [String]
splitByIdxs idxs name =
  map
    (map toLower)
    [ take (end - start) (drop start name)
    | (start, end) <- zip idxs $ tail idxs ++ [length name]
    ]
