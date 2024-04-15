maxIndex :: Ord a => [a] -> (a, Int)
maxIndex xs = maximum (zip xs [0..])

-- xs = elected   ys = votes
calc :: Integral a => [a] -> [a] -> [a]
calc xs ys = [div b1 (a1+1)| (a1,a2) <- zip xs [0..10], (b1,b2) <- zip ys [0..10], b2 == a2]

-- xs = elected   ys = votes
increase :: Integral a => [a] -> [a] -> [a]
increase xs ys = parte1 ++ [parte2 + 1] ++ parte3
                  where (ele1, ele2) = maxIndex (calc xs ys)
                        parte1 = take (ele2) xs
                        parte2 = xs !! (ele2)
                        parte3 = drop (ele2+1) xs

-- xs = elected   ys = votes
my_sum :: (Integral a1, Num a2, Eq a2) => a2 -> [a1] -> [a1] -> [a1]
my_sum x xs ys | x /= 1     =  my_sum (x-1) (increase xs ys) ys
               | x == 1     =  increase xs ys

hondt :: Int -> [Int] -> [Int]
hondt x xs = take (length xs) (my_sum x [0,0..] xs)