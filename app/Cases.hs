module Cases
  ( Case
  , getCase
  , split
  , join
  ) where

import Data.Char

data Case
  = KebabCase
  | PascalCase
  | SnakeCase
  | CamelCase
  | ScreamingSnakeCase
  | ScreamingKebabCase
  deriving (Enum, Show)

getCase :: String -> Case
getCase str
  | containsMinus && isUppercase = ScreamingKebabCase
  | isUppercase = ScreamingSnakeCase
  | containsMinus = KebabCase
  | containsUnderscore = SnakeCase
  | firstLetterIsUpper = PascalCase
  | containsUppercase = CamelCase
  | otherwise = SnakeCase
  where
    containsMinus = '-' `elem` str
    containsUnderscore = '_' `elem` str
    isUppercase = all isUpper str
    firstLetterIsUpper = isUpper $ head str
    containsUppercase = any isUpper str

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
joinPascalCase = concatMap capitalize

splitCamelCase :: String -> [String]
splitCamelCase name = splitByIdxs (0 : upperCaseIdxs name) name

joinCamelCase :: [String] -> String
joinCamelCase [] = ""
joinCamelCase (car:cdr) = car ++ joinPascalCase cdr

upperCaseIdxs :: String -> [Int]
upperCaseIdxs str = map fst $ filter (\(_, c) -> isUpper c) $ enumerate str

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

capitalize :: String -> String
capitalize it = toUpper (head it) : tail it

splitOn :: Char -> String -> [String]
splitOn c str = part : splitOn c (dropWhile (== c) $ drop (length part) str)
  where
    part = takeWhile (/= c) str

joinWith :: Char -> [String] -> String
joinWith _ [] = ""
joinWith _ [car] = car
joinWith c (car:cdr) = car ++ [c] ++ joinWith c cdr

splitByIdxs :: [Int] -> String -> [String]
splitByIdxs idxs name =
  [ map toLower $ take (end - start) (drop start name)
  | (start, end) <- zip idxs $ tail idxs ++ [length name]
  ]
