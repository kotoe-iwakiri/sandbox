import Data.List

main :: IO ()
main = do
    contents <- getContents
    mapM_ (print . countSeven) (lines contents)
--    print ( countSeven ( countSevenInDigitAll 3))

countSeven :: String -> Integer
countSeven x = countSevenInDigitAll (toInteger ((length x) - 1)) + sum( map countSevenInteger [(10 ^ ((length x) - 1)) .. (read x)])

countSevenInteger :: Integer -> Integer
countSevenInteger x
  | x < 10    = if x == 7 then 1 else 0
  | otherwise = (countSevenInteger (x `mod` 10)) + (countSevenInteger (x `div` 10))

countSevenInDigitAll :: Integer -> Integer
countSevenInDigitAll digit = reduce $ map (\x -> (x, (countSevenInDigit digit x))) [1 .. digit]
-- digit桁だったら何通りあるか

reduce :: [(Integer, Integer)] -> Integer
reduce xs = foldl' (\acc (x, y) -> acc + x * y) 0 xs

countSevenInDigit :: Integer -> Integer -> Integer
-- 7をn個含む数は、digit桁までで、何通りか
-- 2種類のもの（n個, digit - n個）の並び替え　かける　異なる9個のもののdigit - n個の並び替え
countSevenInDigit digit n = (permutation2 n (digit - n)) * (permutation (digit - n))

-- ２種類のものの順列 p <= qのほうが計算量少ない
permutation2 :: Integer -> Integer -> Integer
permutation2 p q = (fact (q + 1) (p + q)) `div` (fact 1 p)

permutation :: Integer -> Integer
permutation n 
  | n == 0 = 1
  | otherwise = fact (9 - n + 1) 9

fact :: Integer -> Integer -> Integer
-- (m <= n) n * n - 1 * .. * m
fact m n 
  | n == 0    = 1
  | n == m    = m
  | otherwise = n * fact m (n - 1)

