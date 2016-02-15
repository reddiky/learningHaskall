doubleMe x = x + x
doubleUs x y  = x * 2 + y * 2
doubleSmallNumber  x =  if x > 100 then x else x*2

boomBangs xs = [if x < 10 then "Boom!" else "BANG!" | x<-xs, odd x, x/=3, x/=5, x/=7]


filterLowers st = [c | c <- st, c `elem`  ['A'..'Z']]

sumLowers st = sum [1 | _ <- [c | c <- st, elem c ['a'..'z']]]

rightTriangles = [(a,b,c) |  c <- [1..10], a <- [1..c], b <- [1..a], a ^2 + b ^ 2 == c^2, a+b+c==24]

factorial :: Integer -> Integer
factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference'  r = 2 * pi * r