main = do n <- readLn
          print (fn n)

fn :: Int -> [Int]
fn n = replicate n 1
