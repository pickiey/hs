{-
レーベンシュタイン距離のアルゴリズムを理解する
http://djangoapplab.com/40/

文字列間のレーベンシュタイン距離を求める(3)Haskell版ふたたび | blog.PanicBlanket.com
http://blog.panicblanket.com/archives/3489

Haskellでレーベンシュタイン距離（文字列編集距離）を求める - Qiita
http://qiita.com/tmnck/items/9987ca61d9697da45103
-}

levenshtein_distance :: String -> String -> Int
levenshtein_distance a b = last $ last matrix
    where
        la = length a
        lb = length b
        matrix = [[distance m n | n <- [0..lb]] | m <- [0..la]]
        distance i j
            | i == 0 = i
            | j == 0 = j
            | otherwise = minimum [ matrix!!(i-1)!!j + 1,
                                    matrix!!i!!(j-1) + 1,
                                    matrix!!(i-1)!!(j-1) + cost]
            where
                cost
                    | a!!(i-1) == b!!(j-1) = 0
                    | otherwise            = 1

main = do
    str1 <- getLine
    str2 <- getLine

    print $ levenshtein_distance str1 str2
