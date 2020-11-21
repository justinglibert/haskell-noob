doubleMe x = x * 2
doubleUs x y = doubleMe x + doubleMe y

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)


replicate' :: Integer -> a -> [a]
replicate' 1 x = [x]
replicate' n x = x:replicate' (n - 1) x


repeat' :: a -> [a]
repeat' x = x:repeat' x


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort smaller ++ [x] ++ quicksort bigger
  where smaller = [y | y <- xs, y <= x]
        bigger = [y | y <- xs, y > x]

chain :: Integer -> [Integer]
chain 1 = [1]
chain x = x : chain rest
  where rest = if even x then x `div` 2 else (x * 3) + 1


numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
  where isLong xs = length xs > 15

sum' :: (Num a) => [a] -> a
sum' = foldl (\acc x -> acc + x) 0
