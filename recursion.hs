replicate' :: Int -> a -> [a]
replicate' n x
	| n <= 0 = []
	| otherwise = x : replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [b] -> [b]
take'  n _
	| n <= 0 = []
take'  _ [] = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
	let  smallerOrEqual = [a | a <- xs, a <= x]
	     larger = [a | a <-xs, a > x]
        in quickSort smallerOrEqual ++ [x] ++ quickSort larger    