import Data.Char
import System.IO

-- main = do
--   putStrLn "Hello, what's your name?"
--   name <- getLine
--   putStrLn "What's your last name?"
--   lastName <- getLine
--   let bName = map toUpper name
--       bLastName = map toUpper lastName
--   putStrLn ("Hey " ++ bName ++ " " ++ bLastName ++ ", you rock!")

-- main = do
--   line <- getLine
--   if null line
--     then return ()
--     else do
--       putStrLn $ reverseWords line
--       main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

respondPalindromes :: String -> String
respondPalindromes = unlines . map (\xs -> if isPalindrome xs then "palindrome" else "notPalindrome") . words
  where
    isPalindrome xs = xs == reverse xs

-- main = interact respondPalindromes

main = do
  contents <- readFile "girlfriend.txt"
  putStr contents
