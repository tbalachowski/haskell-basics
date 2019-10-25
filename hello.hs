doubleMe x = x + x
doubleUs x y = x * 2 + y * 2
doubleSmallNumber x = if x > 10 then x else x * 2
doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1
conanO'Brien = "It's a-me, Conan O'Brien!"
length' xs = sum [1 | _ <- xs]
triangles = [(a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10]]
rightTriangles = [(a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a ^2 + b ^ 2 == c ^ 2]
rightTriangles' = [(a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a ^2 + b ^ 2 == c ^ 2, a + b + c == 24]

removeNonUppercase :: String -> String
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

factorial :: Integer -> Integer
factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Float -> Float
circumference' r = 2 * pi * r