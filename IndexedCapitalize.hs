import Data.Char (toUpper)
import Data.List (intercalate)

capitalizeAtIndices :: String -> [Int] -> String
capitalizeAtIndices s indices = 
    [if i `elem` indices then toUpper c else c | (i, c) <- zip [0..] s]

main :: IO ()
main = do
    putStrLn "Enter a string:"
    input <- getLine
    putStrLn "Enter indices to capitalize (comma separated, e.g., 0,2,4):"
    indicesInput <- getLine
    let indices = map read (splitByComma indicesInput) :: [Int]
    let result = capitalizeAtIndices input indices
    putStrLn ("Result: " ++ result)

splitByComma :: String -> [String]
splitByComma = words . map (\c -> if c == ',' then ' ' else c)
