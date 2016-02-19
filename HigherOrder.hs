divideBy5 :: (Floating a) => a -> a
divideBy5 = (/5)


map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map'  f (x:xs) = f x : map f xs


filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
	| f x = x : filter' f xs
	| otherwise = filter' f xs

bigFilter :: Integer
bigFilter = head (filter p [99999,99998..])
	where p x = x `mod` 3829 == 0

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f (x:xs)
	| f x = x : takeWhile f xs
	| otherwise = [x]
	 

collatz :: Integer -> [Integer]
collatz 1 = [1]
collatz a
	| even a = a : collatz (a `div` 2)
	| odd a =  a : collatz (a*3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map collatz [1..100]))
	where isLong xs = length xs > 15


sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1