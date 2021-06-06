import Control.Monad.State

main = interact $ show . solve . head . words

solve :: String -> Int
solve xs = evalState (countAlteredChars xs) 0

countAlteredChars :: String -> State Int Int
countAlteredChars [] = do
    count <- get
    return count

countAlteredChars xs = do
    let [x,y,z] = take 3 xs
    count <- get
    if x /= 'S' then put (count +1) else put count
    count <- get
    if y /= 'O' then put (count +1) else put count
    count <- get
    if z /= 'S' then put (count +1) else put count
    countAlteredChars $ drop 3 xs
