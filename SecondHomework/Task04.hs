type Account = (Int, Int, Double)

type Person = (Int,String,String)

map1 :: (a -> b) -> [a] -> [b]
map1 _ []     = []
map1 f (x:xs) = (f x) : map1 f xs

flatmap _ [] = []  
flatmap f (x:xs) = f x ++ flatmap f xs

average :: [Double] -> Double
average ps = sum ps / (fromIntegral (length ps))

extractFirst :: (a, b, c) -> a
extractFirst (a,_,_) = a

extractSecond :: (a, b, c) -> b
extractSecond (_,b,_) = b

extractThird :: (a, b, c) -> c
extractThird (_,_,c) = c

ps :: [Person]
ps = [(1, "Ivan", "Sofia"), (2, "Georgi", "Burgas"), (3, "Petar", "Plovdiv"), (4, "Petya", "Burgas")]
ps1 :: [Person]
ps1 = [(1, "Ivan", "Sofia"), (2, "Georgi", "Burgas")]
as :: [Account]
as = [(1, 1, 12.5) , (2, 1, 123.2) , (3, 2, 13.0) , (4, 2, 50.2),(5, 2, 17.2), (6, 3, 18.3), (7, 4, 19.4)]







getAccountsForPerson :: [Account] -> Person -> [Account]
getAccountsForPerson accounts person =
    [k | k <- accounts, extractFirst person == extractSecond k]

getAccountsForPeople :: [Account] -> [Person] -> [Account]
getAccountsForPeople as ps =
    flatmap (getAccountsForPerson as) ps

getAverageBalance :: ([Account],[Person]) -> (Person -> Bool) -> Double
getAverageBalance tuple p =

  average (map1 extractThird (getAccountsForPeople (fst tuple) [k | k <- (snd tuple), p k]))

--getBalanceForPerson :: [Account] -> 

ps :: [Person]
ps = [(1, "Ivan", "Sofia"), (2, "Georgi", "Burgas"), (3, "Petar", "Plovdiv"), (4, "Petya", "Burgas")]
ps1 :: [Person]
ps1 = [(1, "Ivan", "Sofia"), (2, "Georgi", "Burgas")]
as :: [Account]
as = [(1, 1, 12.5) , (2, 1, 123.2) , (3, 2, 13.0) , (4, 2, 50.2),(5, 2, 17.2), (6, 3, 18.3), (7, 4, 19.4)]

main::IO()
main = do
  --  print("hh")
   print((getAverageBalance (as,ps) (\ (_,(n:_),_) -> n == 'P')))