main :: IO ()
main = do
    contents <- getContents
    let xs = lines contents
        limits = map read xs :: [Int]
        sevens = map countSevenUntil limits
    mapM_ print sevens
 
countSevenUntil :: Int -> Int
countSevenUntil limit = foldr (\x acc -> acc + countSeven x) 0 [1 .. limit]
 
countSeven :: Int -> Int
countSeven x
  | x < 10    = if x == 7 then 1 else 0
  | otherwise = (countSeven (x `mod` 10)) + (countSeven (x `div` 10))
