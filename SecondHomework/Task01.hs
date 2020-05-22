generateElement :: Float -> Float -> Float
generateElement p n =
    if n == 1 
        then 1
    else (1 / (n ** p))  + (generateElement p (n - 1))

generate :: Float -> Float -> [Float]
generate p n =
    if n == 0 
        then []
    else generate p (n-1) ++ [generateElement p n]

main :: IO()
main = do
   -- print(generateElement 1 3)
    print(generate 1 3)
    print(generate 0.1 5)
   --print(1 : [1,2])