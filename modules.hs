import Data.List
import Data.Char
import qualified Data.Map as Map
numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub


wordNums :: String -> [(String, Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words

isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack)


encode :: Int -> String -> String
encode offset = map (\c -> chr . (+ offset) . ord $ c) 

decode :: Int -> String -> String
decode offset = encode (negate offset)


findKey :: (Eq k) => k -> [(k, v)] -> v
findKey key = snd . head . filter(\(k, _) -> k == key) 


findKey' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey' key = foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing


