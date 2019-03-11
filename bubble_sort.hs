-- http://qiita.com/s-shin/items/b6f3035202611497bf6d

bubblesort :: Ord a => [a] -> [a]
bubblesort [] = []
bubblesort xs = (bubblesort $ init xs') ++ [last xs']
    where
        exchangeNeighbors ys = case ys of
            (a:b:zs) -> (min a b):(exchangeNeighbors $ (max a b):zs)
            (a:_)    -> [a]
        xs' = exchangeNeighbors xs
