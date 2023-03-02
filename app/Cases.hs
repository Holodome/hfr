module Cases where
import Data.Char

enumerate :: [a] -> [(Int, a)]
enumerate it = zip [0..] it

capitalize :: String -> String 
capitalize it = (++) [toUpper (head it)] (tail it)

upper_case_idxs :: String -> [Int]
upper_case_idxs str = 
    map (\(i, _) -> i) (filter (\(_, c) -> isUpper c) (enumerate str))

split_kebab_case :: String -> [String]
split_kebab_case name = words [ if c == '-' then ' ' else c | c <- name ]

join_kebab_case :: [String] -> String 
join_kebab_case [] = ""
join_kebab_case (car:cdr) = car ++ join_kebab_case cdr

split_pascal_case :: String -> [String]
split_pascal_case name = 
    map (map toLower)
    [ take (end - start) (drop start name) | 
        (start, end) <- zip idxs ((tail idxs) ++ [length name]) ]
    where idxs = upper_case_idxs name

join_pascal_case :: [String] -> String
join_pascal_case [] = ""
join_pascal_case (car:cdr) = (capitalize car) ++ join_pascal_case cdr

