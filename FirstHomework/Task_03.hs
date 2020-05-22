--Функция, която проверява дали дадено число е просто.
isPrime :: Int -> Bool
isPrime k = if k > 1 then null [ x | x <- [2..k - 1], k `mod` x == 0] 
    else False

--Функция, която проверява дали всички числа съставени от k съседни цифри на n са прости.
isSpecial :: Int -> Int -> Bool
isSpecial n k = 
    if div n (10 ^ (k - 1)) == 0 
        then True
    else
         isPrime (mod n (10 ^ k)) && isSpecial (div n 10) k

main :: IO()
main = do
    print(isSpecial 131 2)
    --print(isSpecial 472 2)
    --print(isSpecial 12234 3)
    