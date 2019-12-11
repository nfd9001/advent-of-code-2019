main = do
    f <- readFile "day1.txt"
    let d = read <$> lines f
    -- pt. 1
    print $ sum $ (\x -> x `div` 3 + 5) <$> d

    --pt. 2
    let im x = x `div` 3 - 2 
    let fuel x f = if (x < 9)
        then (f)
        else (fuel (im x) (f + im x))
    print $ sum $ ((flip fuel) 0) <$> d
