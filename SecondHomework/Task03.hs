type Point = (Double, Double)

isPointInCircle :: Point -> Double -> Point -> Bool
isPointInCircle p r c =
    sqrt((fst p - fst c) ^ 2 + (snd p - snd c) ^ 2) <= r

splitPoints :: Point -> Double -> [Point] -> ([Point], [Point])
splitPoints p r ps = 
   ( [k | k <- ps, isPointInCircle k r p] , [k | k <- ps, not (isPointInCircle k r p)])

main::IO()
main = do
 print((splitPoints (10,10) 5 [(1,2),(2,3),(10,15),(-1,1),(12,14)]) )