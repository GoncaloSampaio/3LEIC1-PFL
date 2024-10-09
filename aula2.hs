myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs)
    | x = myAnd xs
    | otherwise = False

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs)
   | x = True
   | otherwise = myOr xs

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (x:xs) = x ++ myConcat (xs)

myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []
myReplicate 1 a = [a]
myReplicate x a = [a] ++ myReplicate (x-1) a

myIndex :: [a] -> Int -> a
myIndex [] _ = error "Index too large"
myIndex (a:as) 0 = a
myIndex (a:as) i = myIndex as (i-1)

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a (x:xs)
   | a == x = True
   | otherwise = myElem a xs

myIntersperce :: a -> [a] -> [a]
myIntersperce _ [] = []
myIntersperce _ [a] = [a]
myIntersperce x (a:as) = a : x : myIntersperce x as

mdc :: Integer -> Integer -> Integer
mdc a 0 = a
mdc a b = mdc b (mod a b)

myInsert :: Ord a => a -> [a] -> [a]
myInsert a [] = [a]
myInsert a (x:xs)
    | a >= x = x : myInsert a xs
    | otherwise = a : x : xs

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = myInsert x (isort xs)

myMinimum :: Ord a => [a] -> a
myMinimum [] = error "Empty list"
myMinimum x = myIndex (isort x) 0

myDelete :: Eq a => a -> [a] -> [a]
myDelete _ [] = []
myDelete a (x:xs)
    | a == x = xs
    | otherwise = x : myDelete a xs

ssort :: Ord a => [a] -> [a]
ssort [] = []
ssort x = myMinimum x : ssort (myDelete (myMinimum x) x)

sumSquares :: [Int] -> Int
sumSquares [] = 0
sumSquares (x:xs) = x * x + sumSquares xs

somaSquaresExpression :: Int
somaSquaresExpression = sum [x^2 | x <- [1..100]]
{--x ao quadrado (x ^ 2)sendo que (|) os valores de x vêm de 1 até 100 ([1..100])--}

aprox :: Int -> Double
aprox x = 4.0 * sum [((-1) ^ n) / fromIntegral(2 * n + 1) | n <- [0..x]]

aprox' :: Int -> Double
aprox' x = sqrt (12.0 * sum [((-1) ^ n) / fromIntegral(n + 1) ^ 2 | n <- [0..x]])

dotProd :: [Float] -> [Float] -> Float
dotProd x y = sum [sum c | c <- (zip x y)]

divProp :: Integer -> [Integer]
divProp 0 = []
divProp x = [y | y <- [1..x-1], mod x y == 0]

perfeitos :: Integer -> [Integer]
perfeitos 0 = []
