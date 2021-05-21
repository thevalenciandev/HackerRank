main :: IO ()
main = interact $ solve . last . words

solve :: String -> String
solve (h1:h2:_:m1:m2:_:s1:s2:xs) = 
    case xs of 
      "PM" -> if hour == "12" then 
                toMilitaryTime "12"
             else 
                toMilitaryTime $ show (12 + read hour)
      "AM" -> if hour == "12" then
                toMilitaryTime "00"
              else
                toMilitaryTime (h1:[h2])
    where hour = (h1:[h2])
          toMilitaryTime h = h ++ (':':m1:m2:':':s1:[s2])
