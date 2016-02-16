lucky :: Int -> String
lucky 7 =  "LUCKY NUMBER SEVEN!"
lucky x = "OUT OF LUCK, PAL :-("


factorial :: Int -> Int
factorial 0 = 1;
factorial n = n * factorial (n - 1)

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "The list is long, the first two elements are " ++ show x ++ " and " ++ show y

firstLetter :: String -> String
firstLetter "" = "Oops, empty string!"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

complicatedBMI :: Double -> Double -> String
complicatedBMI weight height
	| bmi <= skinny = "You're underweight, eat more!"
	| bmi <= normal = "Looking Good!"
	| bmi <= fat = "You're a fatty, let's workout!"
	| otherwise = "You're obese, go see a doctor!"
	where skinny = 18.5
	      bmi = weight / height ^ 2
	      normal = 25.0
	      fat = 30.0