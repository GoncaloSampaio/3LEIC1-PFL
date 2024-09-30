import Data.Array (Ix(index))
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
perfeitos x = [y | y <- [1.x], sum divProp y == y]

pitagoricos :: Integer -> [(Integer ,Integer ,Integer)] 
pitagoricos 0 = []
pitagoricos a = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

primo :: Integer -> Bool
primo 0 = False
primo 1 = False
primo x 
    | length divProp x > 2 = False
    | otherwise = True~

mersennes :: [Int]
mersennes = [(2 ^ n -1) | n <- [1..30], primo (2 ^ n - 1)]

{--Função da TP passada--}
binom :: Integer -> Integer -> Integer
binom n k = 
    (product [1..n] `div` (product[1..k] * product[1..n-k]))
{--Função da TP passada--}

pascal :: Integer -> [[Integer]]
pascal n = [[binom i k | k <- [0..i]] | i <- [0..n]]

cifrarChar :: Int -> Char -> Char
cifrarChar n c
    | isLower c = chr ((ord c - ord 'a' + n) `mod` 26 + ord 'a')  
    | isUpper c = chr ((ord c - ord 'A' + n) `mod` 26 + ord 'A')  
    | otherwise = c

cifrar :: Int -> String -> String
cifrar n str = map (cifrarChar n) str

myConcat2 :: [[a]] -> [a]
myConcat2 xs = [y | x <- xs, y <- x ]
{-- lista de "y" sendo que este "y" vem da lista de "x" q por sua vez, sendo este uma lista, vem de uma lista de listas "xs"--}

myReplicate2 :: Int -> a -> [a]
myReplicate2 0 _ = []
myReplicate2 n x = [x | _ <- [1..n]] {--faz x repetir n vezes com algo ("_") que é incrementado, mas não diretamente usado--}

forte :: String -> Bool