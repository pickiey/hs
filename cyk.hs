import Data.List

main = do
  putStr "Input sentence. > "

  input <- getLine

  putStr "\n"

  let cyk_list = cykScan $ initList $ map checkRewriteRule_1 $ map defineSymbol $ words input
      end_message = evaluateOutcome cyk_list

  mapM print cyk_list

  putStr "\n"

  putStr end_message

  putStr "\n"



cykScan :: [[[String]]] -> [[[String]]]
cykScan list = f_1 list 1
  where
    n = length list

    f_1 :: [[[String]]] -> Int -> [[[String]]]
    f_1 list i
      | i ==  n   = list
      | i > 7     = f_1 list ( i + 1 )
      | otherwise = f_1 ( f_2 list i ) ( i + 1 )
    -- リストを更新していく関数
    -- i = 更新回数

    f_2 :: [[[String]]] -> Int -> [[[String]]]
    f_2 list i = zipList list ( f_3 list i )
    -- 元のリストと追加分リストを結合

    f_3 :: [[[String]]] -> Int -> [[[String]]]
    f_3 list i = f_4 list i 1
    -- 追加分リスト(i+1列目のリスト)の定義

    f_4 :: [[[String]]] -> Int -> Int -> [[[String]]]
    f_4 list i m
      | m == ( n - i )  = [ f_5 list i m ]
      | otherwise       = ( f_5 list i m ) : ( f_4 list i ( m + 1 ) )
    -- m行i+1列成分からなるリストをつくる関数

    f_5 :: [[[String]]] -> Int -> Int -> [[String]]
    f_5 list i m = nub $ removeNilList [ f_6 list i m x | x <- [ 1 .. i ] ]
    -- m行i+1列成分を求める関数の定義

    f_6 :: [[[String]]] -> Int -> Int -> Int -> [String]
    f_6 list i m x = checkRewriteRule_2 ( choose list m x ) ( choose list ( m + x ) ( i - x + 1 ) )
    -- m行x列成分とm+x行i-x+1列成分でチェックする関数
    -- マッチしなければ [] を返す

choose :: [[[String]]] -> Int -> Int -> [String]
choose list m i = ( ( list !! ( m - 1 ) ) !! ( i - 1 ) )
-- m行i列成分を取り出す関数

zipList :: [[[String]]] -> [[[String]]] -> [[[String]]]
zipList [] []                 = []
zipList ( x : [] ) []         = x : ( zipList [] [] )
zipList ( x : xs ) []         = x : ( zipList xs [] )
zipList ( x : xs ) ( y : [] ) = ( x ++ y ) : ( zipList xs [] )
zipList ( x : xs ) ( y : ys ) = ( x ++ y ) : ( zipList xs ys )

removeNilList :: [[String]] -> [[String]]
removeNilList [[]]                 = [[]]
removeNilList ( x : [] )           = [x]
removeNilList ( x : ( [] : xs ) )  = removeNilList ( x : xs )
removeNilList ( [] : xs )          = removeNilList xs
removeNilList ( x : xs )          =  x : removeNilList xs

initList :: [String] -> [[[String]]]
initList list = f $ f list
  where
    f :: [a] -> [[a]]
    f []          = [[]]
    f ( x : [] )  = [ x ] : []
    f ( x : xs )  = [ x ] : ( f xs )

defineSymbol :: String -> String
defineSymbol word
  | word == "telescope" = "Noum"
  | word == "time"      = "Noum"
  | word == "piano"     = "Noum"
  | word == "played"    = "V"
  | word == "saw"       = "V"
  | word == "Mary"      = "PN"
  | word == "Jack"      = "PN"
  | word == "Tom"       = "PN"
  | word == "the"       = "Det"
  | word == "a"         = "Det"
  | word == "nice"      = "Adj"
  | word == "long"      = "Adj"
  | word == "with"      = "Prep"
  | word == "for"       = "Prep"
  | otherwise           = "Unknown"

checkRewriteRule_1 :: String -> String
checkRewriteRule_1 symbol
  | symbol == "PN"    = "NP"
  | symbol == "Noum"  = "N"
  | symbol == "V"     = "VP"
  | otherwise       = symbol

checkRewriteRule_2 :: [String] -> [String] -> [String]
checkRewriteRule_2 xs ys = [ checkRewriteRule_3 x y | x <- xs , y <- ys , checkRewriteRule_3 x y /= "" ]

checkRewriteRule_3 :: String -> String -> String
checkRewriteRule_3 symbol_1 symbol_2
  | symbol_1 == "NP"    && symbol_2 == "VP"   = "S"
  | symbol_1 == "NP"    && symbol_2 == "PP"   = "NP"
  | symbol_1 == "Det"   && symbol_2 == "N"    = "NP"
  | symbol_1 == "Det"   && symbol_2 == "N'"   = "NP"
  | symbol_1 == "Noun"  && symbol_2 == "N"    = "N"
  | symbol_1 == "VP"    && symbol_2 == "NP"   = "VP"
  | symbol_1 == "VP"    && symbol_2 == "PP"   = "VP"
  | symbol_1 == "Prep"  && symbol_2 == "NP"   = "PP"
  | symbol_1 == "Adj"   && symbol_2 == "N"    = "N'"
  | otherwise                                 = ""

evaluateOutcome :: [[[String]]] -> String
evaluateOutcome list
  | elem "S" x  = "Analysis success."
  | otherwise   = "Analysis failure."
  where
    x = last $ head list

