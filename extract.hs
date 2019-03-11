extract :: Int -> Int
extract n
  | n == 1    = 1
  | otherwise = n * extract (n-1)

main :: IO()
main = do
  print $ extract 5
