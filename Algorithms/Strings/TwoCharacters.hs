import qualified Data.Set as Set
import Data.List
import Data.Maybe
import Data.Ord

main = interact $ show . solve . last . words

solve :: String -> Int
solve xs = if length sortedresult == 0 then 0 else length (head sortedresult)
    where uniquechars   = Set.toList . Set.fromList $ xs
          uniquepairs   = [(x,y) | (x:rest)<-tails uniquechars, y<-rest]
          allpaircombos = [filterPair p xs | p<-uniquepairs]
          alternatingc  = [combo | combo<-allpaircombos, valid combo]
          sortedresult  = sortBy (flip $ comparing length) alternatingc 

-- Takes a list and leaves only the elements from the pair
filterPair :: Eq a => (a,a) -> [a] -> [a]
filterPair pair = filter (\c -> contains c pair)

contains :: Eq a => a -> (a,a) -> Bool
contains x pair = fst pair == x || snd pair == x

-- Determines if xs is alternated
-- then "abab" and "bab" are valid, but "aab"
-- It assumes the list contains only 2 distinct characters 
valid :: Eq a => [a] -> Bool
valid = all (\xs -> length xs == 1) . group 
