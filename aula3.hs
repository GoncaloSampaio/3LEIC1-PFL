{--
	Função de ordem superior:
	- se tiver um argumento como função (- map (^2) [1,2,3] = [1,4,9] -)
		ou
	- se o seu resultado é uma função

	Exemplos:
		- map (aplica uma função a um conjunto de elementos) 
		- filter (filtra, consoante uma condição verificada)
		- takeWhile, dropWhile (seleciona, remove, respetivamente, elementos que verificam um predicado)
		- all, any (verifica se ou todos, ou algum elemento, respetivamente, obecedem a um predicado)
		- foldr, foldl (recebe a função a aplicar, um valor inicial e o argumento aos qual aplicaremos a função)
		- (.) (composição) x . y . z = x { y [ z( ) ] }

	existem listas infinitas: com cycle, repeat, iterate
	podemos tirar x elementos dessas listas com:
		take -> take 3 repeat 'a' = "aaa"
		     -> take 5 cycle [-1,1] = [-1,1,-1,1,-1]
			 -> take 4 (iterate (2*) 1) = [1,2,4,8]
--}

{-- Exercicio 1: [f x | x ← xs, p x] --}

x :: a -> a
x a = a

p :: a -> Bool
p _ = True

f :: [a] -> [a]
f a = map x (filter p a)

{-- Exercicio 2 --}

dec2int :: [Int] -> Int
dec2int = foldl (\acumulador x -> acumulador * 10 + x) 0 {-- lambeda, x representa o elemento atual da lista, o fold trata disso --}

{-- Exercicio 3 --}

myZipWith :: (a -> b-> c) -> [a] -> [b] -> [c]
myZipWith _ _ [] = []
myZipWith _ [] _ = []
myZipWith aux (x:xs) (y:ys) = (aux x y) : myZipWith aux xs ys

{-- Exercicio 4 (isort a usar foldr e insert) --}

myInsert :: Ord a => a -> [a] -> [a]
myInsert a [] = [a]
myInsert a (x:xs)
    | a >= x = x : myInsert a xs
    | otherwise = a : x : xs

isort :: Ord a => [a] -> [a]
isort = foldr myInsert []

{-- Exercicio 5 --}

myMaximum :: Ord a => [a] -> a
myMaximum = foldl1 max

myMinimum :: Ord a => [a] -> a
myMinimum = foldr1 min

myFoldl1 :: (a -> a -> a) -> [a] -> a
myFoldl1 _ [] = error "sim"
myFoldl1 f xs = foldl f (head xs) (tail xs) 

myFoldr1 :: (a -> a -> a) -> [a] -> a
myFoldr1 _ [] = error "sim"
myFoldr1 g xs = foldr g (last xs) (init xs)

{-- Exercicio 6 -- until :: (a-> Bool) -> (a-> a) -> a -> a --}

mdc :: Int -> Int -> Int
mdc a b = fst (until (\(x, y) -> y == 0) (\(x, y) -> (y, x `mod` y)) (a, b))

{-- Exercicio 8 --}

palavras :: String -> [String]
palavras "" = []
palavras xs = palavra : palavras resto
  where
    aux = dropWhile (== ' ') xs
    palavra = takeWhile (/= ' ') aux
    resto = dropWhile (/= ' ') aux

despalavras :: [String] -> String
despalavras [] = ""
despalavras [x] = x
despalavras (x:xs) = x ++ " " ++ despalavras xs