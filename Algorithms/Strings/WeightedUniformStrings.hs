import qualified Data.Set as Set
import Control.Monad.State
import Data.Char

main = do s  <- getLine
          n  <- readLn
          qs <- sequence (replicate n (read <$> getLine))
          let ws = calcweights s
          mapM_ putStrLn (solve qs ws)

solve :: [Int] -> Set.Set Int -> [String]
solve qs ws = foldr (\q acc -> if Set.member q ws then "Yes":acc else "No":acc) [] qs

-- State keeps track of weights, last character processed and how many times
type WeightState = (Set.Set Int, Char, Int)
-- And we return just the weights
type WeightValue = Set.Set Int

startState = (Set.empty,'-',1)

calcweights :: String -> Set.Set Int
calcweights xs = evalState (weights xs) startState

weights :: String -> State WeightState WeightValue
weights [] = do
    (ws, _, _) <- get
    return ws

weights (x:xs) = do
    (ws, lastC, lastCC) <- get
    if x /= lastC then
        let newState = Set.insert (calcweight x) ws
        in  put (newState, x, 1)
    else
        let newState = Set.insert (calcweight x + calcweight lastC * lastCC) ws
        in  put (newState, lastC, lastCC+1)
    weights xs

calcweight :: Char -> Int
calcweight x = ord x - ord 'a' + 1 

