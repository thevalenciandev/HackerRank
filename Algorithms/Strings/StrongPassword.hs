import qualified Data.Set as Set
import Control.Monad.State
import Data.Char

main = interact $ show . solve . last . words

specialchars = Set.fromList "!@#$%^&*()-+"
type PasswordState = [Bool]

solve :: String -> Int
solve xs = max criteriaMissing lengthMissing
    where criteriaMissing = evalState (minimumCriteria xs) [False,False,False,False]
          lengthMissing   = 6 - (length xs) 

minimumCriteria :: String -> State PasswordState Int
minimumCriteria [] = do
    mask <- get
    let criteriadone = foldl (\acc x -> if x then acc+1 else acc) 0 mask
    return (length mask - criteriadone) 

minimumCriteria (x:xs) = do
    s <- get
    let all@[dg,lc,uc,sc] = s
    if isDigit x then 
        put [True,lc,uc,sc]
    else if isLower x then 
        put [dg,True,uc,sc]
    else if isUpper x then 
        put [dg,lc,True,sc]
    else if x `Set.member` specialchars then
        put [dg,lc,uc,True]
    else 
        put all
    minimumCriteria xs
