isSquareRoot:: Int -> Int -> Bool
isSquareRoot x y = 
  if y == 0 
    then False
  else 
    y * y == x || isSquareRoot x (y - 1)

isSquare:: Int -> Bool
isSquare x =
  if x == 1 
    then True
  else 
    isSquareRoot x (div x 2)

map1 :: (a -> b) -> [a] -> [b]
map1 _ []     = []
map1 f (x:xs) = (f x) : map1 f xs

sum1 :: Num t => [t] -> t
sum1 []     = 0
sum1 (x:xs) = x + sum1 xs

listSquaresInInterval:: Int -> Int -> [Int]
listSquaresInInterval a b =
     [k | k <- [a..b], isSquare k]

sumOfSquaresOfDividers:: Int -> Int
sumOfSquaresOfDividers k =
   sum1 (map1 (^ 2) [ x | x <- [1..k], k `mod` x == 0])

createTuple::Int -> (Int, Int)
createTuple a =
    (a, sumOfSquaresOfDividers a)

listSquares:: Int -> Int -> [(Int,Int)]
listSquares a b =
   map1 createTuple (listSquaresInInterval a b)

main::IO()
main = do
    print(listSquares 250 300)

