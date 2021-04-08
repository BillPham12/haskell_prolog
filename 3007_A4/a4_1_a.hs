countZeroFree :: [Int] -> Int
countZeroFree [] = 0
countZeroFree (h:t)
              | checkContainZero h == False = 1+ countZeroFree t
              | otherwise = countZeroFree t



checkContainZero :: Int -> Bool
checkContainZero num
                | num == 10 = True
                | num < 10 && num == 0 = True
                | num > 10 && num <= 100 = checkNum num 0
                | num > 10 && (num `div` 10)*10 == num = True
                | num > 10 = checkContainZero (num `div` 10)
                | otherwise = False

checkNum :: Int -> Int -> Bool
checkNum num ze
        | ze <= (num `div` 10) && ze*10 == num = True
        | ze > (num `div` 10) = False
        | otherwise = checkNum num (ze+1)
