testaTriangulo :: Float -> Float -> Float -> Bool
testaTriangulo a b c = (a < b + c) && (b < a + c) && (c < a + b)

areaTriangulo :: Float -> Float -> Float -> Float
areaTriangulo a b c =
    let s = sum[a,b,c] / 2
    in sqrt(s * (s-a) * (s-b) * (s-c))

metades :: [a] -> ([a],[a]) 
metades xs =
    let mid = (length xs) `div` 2
    in (take mid xs, drop mid xs)

mylast :: [a] -> a
mylast xs = head (reverse xs)

mylast2 :: [a] -> a 
mylast2 xs = 
    let l = length xs - 1
    in head (drop l xs)

myinit :: [a] -> [a]
myinit xs = 
    let l = length xs
    in take (l-1) xs

myinit2 :: [a] -> [a]
myinit2 xs = 
    reverse(drop 1 (reverse xs))

xor :: Bool -> Bool -> Bool
xor a b = (a && not b) || (not a && b)

convert_aux3 :: Int -> String --converter milhares
convert_aux3 i 
    | i < 1000 = convert_aux2 i
    | i >= 1000000 = error "Invalid input"
    | i == 1000 && i < 1000000 = "mil"
    | i > 1000 && i < 1000000 && (i `mod` 1000) /= 0 = convert_aux2 (i `div` 1000) ++ " mil, " ++ convert_aux2 (i `mod` 1000)
    | i > 1000 && i < 1000000 && (i `mod` 1000) == 0 = convert_aux2 (i `div` 1000) ++ " mil"

convert_aux2 :: Int -> String --inferiores a 1000
convert_aux2 i
    | i < 0 || i >= 1000 = error "invalid input"
    | i < 100 && i > 0 = convert_aux i
    | i == 100 = "cem"
    | i > 100 && (i `mod` 100) /= 0 && i < 200 = "cento e " ++ convert_aux (i `mod` 100)
    | i > 100 && (i `mod` 100) == 0 && i < 200 = "cem"
    | i > 200 && (i `mod` 100) /= 0 && i < 300 = "duzentos e " ++ convert_aux (i `mod` 100)
    | i >= 200 && (i `mod` 100) == 0 && i < 300 = "duzentos"
    | i > 300 && (i `mod` 100) /= 0 && i < 400 = "trezentos e " ++ convert_aux (i `mod` 100)
    | i >= 300 && (i `mod` 100) == 0 && i < 400 = "trezentos"
    | i > 400 && (i `mod` 100) /= 0 && i < 500 = "quatrocentos e " ++ convert_aux (i `mod` 100)
    | i >= 400 && (i `mod` 100) == 0 && i < 500 = "quatrocentos"
    | i > 500 && (i `mod` 100) /= 0 && i < 600 = "quinhentos e " ++ convert_aux (i `mod` 100)
    | i >= 500 && (i `mod` 100) == 0 && i < 600 = "quinhentos"
    | i > 600 && (i `mod` 100) /= 0 && i < 700 = "seiscentos e " ++ convert_aux (i `mod` 100)
    | i >= 600 && (i `mod` 100) == 0 && i < 700 = "seiscentos"
    | i > 700 && (i `mod` 100) /= 0 && i < 800 = "setecentos e " ++ convert_aux (i `mod` 100)
    | i >= 700 && (i `mod` 100) == 0 && i < 800 = "setecentos"
    | i > 800 && (i `mod` 100) /= 0 && i < 900 = "oitocentos e " ++ convert_aux (i `mod` 100)
    | i >= 800 && (i `mod` 100) == 0 && i < 900 = "oitocentos"
    | i > 900 && (i `mod` 100) /= 0 && i < 1000 = "novecentos e " ++ convert_aux (i `mod` 100)
    | i >= 900 && (i `mod` 100) == 0 && i < 1000 = "novecentos"

convert_aux :: Int -> String -- inferiores a 100
convert_aux i
    | i < 0 || i >= 100 = error "invalid input"
    | i == 0 = ""
    | i == 1 = "um"
    | i == 2 = "dois"
    | i == 3 = "tres"
    | i == 4 = "quatro"
    | i == 5 = "cinco"
    | i == 6 = "seis"
    | i == 7 = "sete"
    | i == 8 = "oito"
    | i == 9 = "nove"
    | i == 10 = "dez"
    | i == 11 = "onze"
    | i == 12 = "doze"
    | i == 13 = "treze"
    | i == 14 = "catorze"
    | i == 15 = "quinze"
    | i == 16 = "dezasseis"
    | i == 17 = "dezassete"
    | i == 18 = "dezoito"
    | i == 19 = "dezanove"
    | i >= 20 && i < 30 && (i `mod` 10) /= 0 = "vinte e " ++ convert_aux (i `mod` 10)
    | i >= 20 && i < 30 && (i `mod` 10) == 0 = "vinte"
    | i >= 30 && i < 40 && (i `mod` 10) /= 0 = "trinta e " ++ convert_aux (i `mod` 10)
    | i >= 30 && i < 40 && (i `mod` 10) == 0 = "trinta"
    | i >= 40 && i < 50 && (i `mod` 10) /= 0 = "quarenta e " ++ convert_aux (i `mod` 10)
    | i >= 40 && i < 50 && (i `mod` 10) == 0 = "quarenta"
    | i >= 50 && i < 60 && (i `mod` 10) /= 0 = "cinquenta e " ++ convert_aux (i `mod` 10)
    | i >= 50 && i < 60 && (i `mod` 10) == 0 = "cinquenta"
    | i >= 60 && i < 70 && (i `mod` 10) /= 0 = "sessenta e " ++ convert_aux (i `mod` 10)
    | i >= 60 && i < 70 && (i `mod` 10) == 0 = "sessenta"
    | i >= 70 && i < 80 && (i `mod` 10) /= 0 = "setenta e " ++ convert_aux (i `mod` 10)
    | i >= 70 && i < 80 && (i `mod` 10) == 0 = "setenta"
    | i >= 80 && i < 90 && (i `mod` 10) /= 0 = "oitenta e " ++ convert_aux (i `mod` 10)
    | i >= 80 && i < 90 && (i `mod` 10) == 0 = "oitenta"
    | i >= 90 && (i `mod` 10) /= 0 = "noventa e " ++ convert_aux (i `mod` 10)
    | i >= 90 && (i `mod` 10) == 0 = "noventa"    

converte :: Int -> String
converte i
    | i < 1000000 = convert_aux3 i
    | i >= 1000000 && (i `div` 1000000) == 1 && (i `mod` 1000000) == 0 = "um milhao" 
    | i > 1000000 && (i `div` 1000000) == 1 && (i `mod` 1000000) /= 0 = "um milhao e " ++ convert_aux3(i `mod` 1000000)
    | i > 1000000 && (i `div` 1000000) /= 1 && (i `mod` 1000000) == 0 = convert_aux (i `div` 1000000) ++ " milhoes"
    | i > 1000000 && (i `div` 1000000) /= 1 && (i `mod` 1000000) /= 0 = convert_aux (i `mod` 1000000) ++ " milhoes, " ++ convert_aux3(i `mod` 1000000)