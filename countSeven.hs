import Data.List

main :: IO ()
main = do
    contents <- getContents
    mapM_ print $ countSevenAll . lines $ contents

countSevenAll :: [String] -> [Int]
countSevenAll = map $ countSevenUntil . read

countSevenUntil :: Int -> Int
countSevenUntil limit = foldl' (\acc x -> acc + countSeven x) 0 [1 .. limit]

countSeven :: Int -> Int
countSeven x
  | x < 10    = if x == 7 then 1 else 0
  | otherwise = (countSeven (x `mod` 10)) + (countSeven (x `div` 10))
