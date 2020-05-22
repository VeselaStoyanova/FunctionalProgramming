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

main::IO()
main = do 
  print(isSquare 256)
  --print(isSquare 15)

