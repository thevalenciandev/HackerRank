main = interact $ show . sum . map read . words . last . lines
