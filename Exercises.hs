import Data.Char

classifica :: Int -> String
classifica x = if x <= 9 then "reprovado" else
    if x <= 12 then "suficiente" else
        if x <= 15 then "bom" else
            if x <= 18 then "muito bom" else
                "muito bom com distinção"

classifica2 :: Int -> String
classifica2 x   | x <= 9      ="reprovado"
                | x <= 12     ="suficiente"
                | x <= 15     ="bom"
                | x <= 18     ="muito bom"
                | x <= 20     ="muito bom com distinção"

              
classificaimc :: Float -> Float -> String
classificaimc x y   | imc <  18.5   = "baixo peso"
                    | imc <  25     = "peso normal"
                    | imc <  30     = "excesso de peso"
                    | otherwise     = "obesidade"
                    where imc = x/y^2
                    

max3, min3 :: Ord a => a -> a -> a -> a
max3 a b c | a >= b && a >= c     = a
           | b >= a && b >= c     = b
           | otherwise            = c

min3 a b c | a <= b && a <= c     = a
           | b <= a && b <= c     = b
           | otherwise            = c

max4, min4 :: Ord a => a -> a -> a -> a
max4 a b c | a > r    = a
           | otherwise = r
            where r = max b c

min4 a b c | a < r    = a
           | otherwise = r
            where r = min b c


xor :: Bool -> Bool -> Bool
xor x y | x == False && y == False   = False
        | x == True && y == True     = False
        | otherwise                  = True


safetail :: [a] -> [a]
safetail x = if length x == 0 then [] else tail x


safetail2 :: [a] -> [a]
safetail2 x | length x == 0     = []
            | otherwise    = tail x

safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 (_:xs) = xs

curta :: [a] -> Bool
curta xs | length xs == 0 = True
         | length xs == 1 = True
         | length xs == 2 = True
         | otherwise      = False


-- não modifique estas definições!
type Letras = (Char,Char)          -- um bloco de letras
type Digitos = (Int,Int)           -- um bloco de algarismos
type Matricula = (Letras, Digitos, Letras)  -- uma matrícula

letras :: Letras -> Bool
letras (x,y) = if ( x >= 'A' && x <= 'Z') && (y >= 'A' && y <='Z') then True else False

digitos :: Digitos -> Bool
digitos (x,y) = if (x >= 0 && x <= 9) && (y >= 0 && y <= 9) then True else False

-- complete esta definição
valida :: Matricula -> Bool
valida (x,y,z) = if letras x && letras z && digitos y then True else False

converte1 :: (Char,Char) -> Int
converte1 (x,y) = a * 26 + b
               where a = ord x - 65
                     b = ord y - 65

converte2 :: (Int,Int) -> Int
converte2 (x,y) = 10 * x + (y) 


converte3 :: (Char,Char) -> Int
converte3 (x,y) = a * 26 + b
               where a = ord x - 65
                     b = ord y - 64

converte :: Matricula -> Int
converte (x,y,z) = converte1 x * 67600 + converte2 y * 676 + converte3 z

converte4 :: Int -> (Char, Char)
converte4 x = (chr a, chr b)
            where a = ((n `div` 26) `mod` 26) + 65
                  b = n `mod` 26 + 65
                  n = (x-1) `div` 67600

converte5 :: Integral b => b -> (b, b)
converte5 x = (a,b)
            where a = k `mod` 10
                  b = n `mod` 10
                  n = (x-1) `div` 676
                  k = (x-1) `div` 6760

converte6 :: Int -> (Char, Char)
converte6 x = (chr a, chr b)
            where a = ((n `div` 26) `mod` 26) + 65
                  b = n `mod` 26 + 65
                  n = (x-1) `mod` 67600

desconverte :: Int -> Matricula
desconverte x = (converte4 x, converte5 x, converte6 x)

incrMatricula :: Matricula -> Matricula
incrMatricula x = desconverte(converte x + 1)

insert :: Ord a => a -> [a] -> [a]
insert a [] = [a]
insert a (x:xs)  | a < x    = a : x : xs
                 |otherwise = x : insert a xs

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) | xs == []  = (insert x xs)
             | otherwise = insert x (isort xs)


minim :: Ord a => [a] -> a
minim [x] = x
minim (x:y:xs) | x >= y         = minim (y:xs)
               | otherwise      = minim (x:xs)

delete :: Eq a => a -> [a] -> [a]
delete a (x:xs) | x == a     = xs
                | otherwise  = x : delete a (xs)

ssort :: Ord a => [a] -> [a]
ssort [] = []
ssort xs | otherwise = a : ssort(delete a xs)
         where a = minim xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys) | x >= y    = y: merge (x:xs) ys
                    | otherwise = x: merge xs (y:ys)

moedas = [200,100,50,20,10,5,2,1]
trocos :: Int -> [Int]
trocos 0 = []
trocos n = m : trocos (n-m)
        where m = go n moedas
              go n (x:xs)
               | x > n     = go n xs
               | otherwise = x
              
forte :: String -> Bool
forte x = if length x >= 8 && length [ a | a <- x, isUpper a] > 0 && length [ a | a <- x, isLower a] > 0 && length [ a | a <- x, a >= '0' && a <= '9'] > 0 then True else False

myand :: [Bool] -> Bool
myand [] = True
myand (x:xs) = x && myand(xs) 

myor :: [Bool] -> Bool
myor [] = False
myor (x:xs) = x || myor(xs)

intersperse :: a -> [a] -> [a]
intersperse a [] = []
intersperse a [x] = [x]
intersperse a (x:xs) = x : a : intersperse a (xs)

fromBits :: [Int] -> Int
fromBits [] = 0
fromBits (x:xs) = if x == 1 then 2^a + fromBits xs else fromBits xs
            where a = length (x:xs) - 1

zipwith1 :: (t1 -> t2 -> a) -> [t1] -> [t2] -> [a]
zipwith1 f (x:xs) (y:ys) = f x y : zipwith1 f xs ys
zipwith1 _ _ _ = []

palavras :: String -> [String]
palavras [] = []
palavras xs | head xs == ' '  = palavras (dropWhile (\n -> n == ' ') xs)
            | otherwise       = takeWhile (\n -> n /= ' ') xs : palavras (dropWhile (\n -> n /= ' ') xs)

aproxPi1 :: Int -> Double
aproxPi1 n = sum (take n (zipWith (/) (cycle[4,-4]) [1,3..]))

aproxPi2 :: Int -> Double
aproxPi2 0 = 0
aproxPi2 n = 3 + sum (zipWith (/) (cycle[4,-4]) (denominadores (n-1)))

produto :: Int -> Double
produto n = fromInteger(toInteger(n*(n+1)*(n+2)))

denominadores :: Int -> [Double]
denominadores n = take n (map produto [2,4..])

binom :: Int -> Int -> Int
binom n 0 = 1
binom n k | n == k   = 1
binom n k = binom (n-1) (k-1) + binom (n-1) (k) 

pascal :: [[Integer]]
pascal = [[toInteger (binom a b)| let a = x, b <- [0..a]] | x <- [0..]]

primos :: Integer -> [Integer]
primos n = crivo [2..n]

crivo :: [Integer] -> [Integer]
crivo [] = []
crivo (p:xs) = p : crivo [x | x<-xs, x`mod`p/=0]

goldbach :: Integer -> (Integer, Integer)
goldbach n = head [ (x,x') | x <- primos n , x' <- primos n, x + x' == n]


and1 :: [Bool] -> Bool
and1 [] = True
and1 (x:xs) = x && and1 xs
 
or1 :: [Bool] -> Bool
or1 [] = False
or1 (x:xs) = x || or1 xs

concat1 :: [[a]] -> [a]
concat1 [] = []
concat1 (x:xs) = x ++ concat1 xs

replicate1 :: Int -> a -> [a]
replicate1 0 _ = []
replicate1 n x = x : replicate1 (n-1) x

(!!!) :: [a] -> Int -> a
(!!!) xs 0 = head xs
(!!!) (x:xs) n = (!!!) (xs) (n-1)

elem1 :: Eq a => a -> [a] -> Bool
elem1 n [] = False
elem1 n (x:xs) | n == x    = True
              | otherwise  = elem1 n xs

intersperse1 :: a -> [a] -> [a]
intersperse1 _ [] = []
intersperse1 a (x:xs) = x : intersperse1 a xs
      
algarismos_aux :: Int -> [Int] 
algarismos_aux 0 = []
algarismos_aux x = a : algarismos_aux n
      where a = mod x 10
            n = div x 10

algarismos :: Int -> [Int] 
algarismos x = reverse $ algarismos_aux x

fromBits1 :: [Int] -> Int
fromBits1 [] = 0
fromBits1 (x:xs) = 2^exp * x + fromBits1 xs
            where exp = length (x:xs) - 1

insert1 :: Ord a => a -> [a] -> [a]
insert1 n [] = [n]
insert1 n (x:xs) | x < n  = x : insert1 n xs
                 | otherwise = n : (x:xs)

isort1 :: Ord a => [a] -> [a]
isort1 [] = []
isort1 (x:xs) = insert1 x (isort1 xs)

delete1 :: Eq a => a -> [a] -> [a]
delete1 n [] = []
delete1 n (x:xs) | x == n    = xs
                 | otherwise = x : delete1 n xs

divisores :: Int -> [Int]
divisores n = filter (\x -> mod n x == 0) [2.. div n 2 + 1 ]

primo :: Integer -> Bool
primo 1 = False
primo n = not (any (\x -> mod n x == 0)  [2..floor(sqrt (fromIntegral n))])

(+++) :: [a] -> [a] -> [a]
(+++) xs ys = foldr (\n res -> n : res) ys xs

concat2 :: [[a]] -> [a]
concat2 xs = foldr (\n res -> n ++ res) [] xs

reverse3 :: [a] -> [a]
reverse3 xs = foldl (\n res -> res : n) [] xs

reverse4 :: [a] -> [a]
reverse4 xs = foldr (\n res -> res ++ [n]) [] xs

elem2 :: Eq a => a -> [a] -> Bool
elem2 n xs = any (\k -> k == n) xs

zipWith2 f _ [] = []
zipWith2 f [] _ = []
zipWith2 f (x:xs) (y:ys) = f x y : zipWith2 f xs ys 

scanl2 :: (b -> a -> b) -> b -> [a] -> [b]
scanl2 f z [] = [z]
scanl2 f z (x:xs) = z : scanl2 f (f z x) xs


aproxPi3 n = sum [ q/i | (q,i) <- zip qua imp]
      where qua = cycle [4,-4]
            imp = [1,3..(2*n -1)]


aux n = [(a',b',c') | a' <- [0..n], b' <- [0..n], c' <- [0..n], a' + b' + c' == n]

hamming = take 100 [2^i*3^j*5^k |  n <- [0..], (i,j,k) <- aux n ]

rot13 n | isLower n  = chr (mod (ord n - ord 'a' + 13) 26 + ord 'a')
        | isUpper n = chr (mod (ord n - ord 'A' + 13) 26 + ord 'A')

main :: IO ()
main = do
    input <- getContents
    let a = map rot13 input
    putStr (a)

--code message
lettertonum :: [Char] -> [Int]
lettertonum xs = [ ord x - ord 'A'| x <- xs]

rotx :: Char -> Int -> Char
rotx letra num = chr (mod (ord letra - ord 'A' + num) 26 + ord 'A')

vigenere :: String -> String -> String
vigenere xs ys = [ rotx x c | (x,c) <- zip ys cod]
            where cod = cycle (lettertonum xs)

-- decode message

rotinverse :: Char -> Int -> Char
rotinverse letra num = chr (mod (ord letra - ord 'A' - num) 26 + ord 'A')

decode :: String -> String -> String
decode xs ys = [ rotinverse x c | (x,c) <- zip ys cod]
            where cod = cycle (lettertonum xs)
