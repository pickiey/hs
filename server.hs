-- http://labs.gree.jp/blog/2013/12/9882/



-- C

void main() {
  型 var = 初期値;
  server(var);
}

void server(型 var) {
  while(true) {
    varを更新する処理;
  }
}



-- Haskell

main :: IO ()
main = do
  var <- newTVarIO 初期値
  server var

server :: TVar 型 -> IO()
server var = loop
  where
    loop = do
      varを更新する処理
      loop
