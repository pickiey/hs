module Main where
import Control.Applicative ((<$>))
import System.Environment (getArgs)

solve 0 = [([],[],[])]
solve n = -- n-1枚をaからcに移す一連の遷移 solve (n-1) がわかっているとするとそれに対し
    map (\(a,b,c)->(a++[n],c,b)) (solve (n-1)) ++ -- 柱aにnをくっつけて柱bと柱cを交換
    map (\(a,b,c)->(b,a,c++[n])) (solve (n-1))    -- 柱cにnをくっつけて柱aと柱bを交換



main = solve <$> read <$> head <$> getArgs >>= mapM_ print



-- Haskellでハノイの塔 - Qiita
-- http://qiita.com/lex_naturalis/items/5c226d60d8e89df63273
