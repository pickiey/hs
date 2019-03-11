import System.IO
import System.Random

zun:: Int -> [Int] -> String
zun cnt (0:ran) = "ズン" ++ zun (cnt+1) ran
zun cnt (r:ran)
  | cnt > 3 = "ドコキ・ヨ・シ!"
  | otherwise = "ドコ" ++ zun 0 ran



main :: IO()
main = do
  gen <- getStdGen
  let ran = randomRs(0,1) gen::[Int]
  putStrLn $ zun 0 ran
