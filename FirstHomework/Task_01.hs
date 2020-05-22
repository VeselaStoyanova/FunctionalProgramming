findSumOfBs:: Int -> Int -> Int
findSumOfBs b n =
    if n == 0 
        then b
    else 
        b * (2 ^ n) + findSumOfBs b (n - 1) 

findSum:: Int -> Int -> Int -> Int
findSum a b n = 
    --Намираме сумата на последните 3 елемента от редицата.
    a + (findSumOfBs b (n - 1)) + a + (findSumOfBs b (n - 2)) + a + (findSumOfBs b (n - 3))

main::IO()
main = do
    print(findSum 0 2 10)
    --print(findSum 5 3 5)

    
    
