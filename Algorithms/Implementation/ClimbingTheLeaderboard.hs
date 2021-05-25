import Data.List

solve :: [Int] -> [Int] -> [Int]
solve board scores = [calcrank s | s <- scores]
    where flatboard      = map head $ group board
          initialpos     = length flatboard + 1
          calcrank score = foldr (\x acc -> if x <= score then acc-1 else acc) initialpos flatboard
          
