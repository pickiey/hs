--module Main where
--import Data.List (unwords)
--import Control.Applicative ((<$>))
import System.Environment (getArgs)

type Tower = ([Int],[Int],[Int])
type Move = Tower -> Tower

-- :: Move
aTb (a:as, bs, cs) = (as, a:bs, cs)
aTc (a:as, bs, cs) = (as, bs, a:cs)
bTc (as, b:bs, cs) = (as, bs, b:cs)
bTa (as, b:bs, cs) = (b:as, bs, cs)
cTa (as, bs, c:cs) = (c:as, bs, cs)
cTb (as, bs, c:cs) = (as, c:bs, cs)

-- :: Int -> [Move]
moveAtoC n
    | n == 1    = [aTc]
    | otherwise = moveAtoB (n-1) ++ moveAtoC 1 ++ moveBtoC (n-1)

moveAtoB n
    | n == 1    = [aTb]
    | otherwise = moveAtoC (n-1) ++ moveAtoB 1 ++ moveCtoB (n-1)

moveBtoC n
    | n == 1    = [bTc]
    | otherwise = moveBtoA (n-1) ++ moveBtoC 1 ++ moveAtoC (n-1)

moveBtoA n
    | n == 1    = [bTa]
    | otherwise = moveBtoC (n-1) ++ moveBtoA 1 ++ moveCtoA (n-1)

moveCtoA n
    | n == 1    = [cTa]
    | otherwise = moveCtoB (n-1) ++ moveCtoA 1 ++ moveBtoA (n-1)

moveCtoB n
    | n == 1    = [cTb]
    | otherwise = moveCtoA (n-1) ++ moveCtoB 1 ++ moveAtoB (n-1)

run :: Tower -> [Move] -> [Tower]
run = scanl (flip ($))

solve :: Int -> [Tower]
solve n
    | n == 0    = [([], [], [])]
    | otherwise = run ([1..n],[],[]) (moveAtoC n)



main = do
    line <- getArgs
    let ts = solve $ read $ head line
    mapM_ print ts
    print $ (length ts) - 1



-- Haskellでハノイの塔 - Qiita
-- http://qiita.com/lex_naturalis/items/5c226d60d8e89df63273
