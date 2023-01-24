add10toall :: [Int] -> [Int]
add10toall x = map (\y -> y + 10) x

multN' :: Int -> [Int] -> [Int]
multN' x n = map (\y -> y * x) n

multN :: Int -> [Int] -> [Int]
multN x n =  [y*x | y <- n]

applyExpr :: [Int] -> [Int]
applyExpr y = [3*x+2 | x <- y]

applyExpr' :: [Int] -> [Int]
applyExpr' x = map (\y -> 3*y+2) x

addSuffix :: String -> [String] -> [String]
addSuffix sf str = [sf ++ x | x <- str]

selectgt5 :: [Int] -> [Int]
selectgt5 y = [ x | x <- y, x > 5]

sumOdds :: [Int] -> Int
sumOdds som = sum [ x | x <- som, mod x 2 /= 0]

sumOdds' :: [Int] -> Int
sumOdds' som = sum (filter (\y -> mod y 2 /= 0) som)

selectExpr :: [Int] -> [Int]
selectExpr y =  [ x | x <- y, x > 20,x < 50, mod x 2 == 0]

countShorts :: [String] -> Int
countShorts str = length [ x | x <- str, length x < 5 ]

calcExpr :: [Float] -> [Float]
calcExpr y = [ x | x <- map (\f -> f^2/2) y, x > 10]

trSpaces :: String -> String
trSpaces y = [ if x == ' ' then '-' else x | x <- y]

selectSnd :: [(Int,Int)] -> [Int]
selectSnd y = [ snd x| x <- y]

--sumAll :: (int,int) -> int
--sumAll x = sum x

dotProd :: [Int] -> [Int] -> Int
dotProd x y = sum [z*t|(z,t) <- (zip x y)] 

main = do
  putStrLn "Hello"
  putStrLn "World"