import Data.Array.IO
import Data.Array.MArray
main = do
    f <- readFile "day2.txt"
    w <- return $ (read :: String -> Int) <$> (words $ (\x -> if x == ',' then ' ' else x) <$> f)
    a <- newListArray (0, length w - 1) w :: IO (IOArray Int Int)
    writeArray a 1 12
    writeArray a 2 2
    res <- performNext a 0
    putStrLn $ show $ res

performNext arr ind = let 
    op 1 x y target = writeArray arr target (x + y) 
    op 2 x y target = writeArray arr target (x * y)
    op i _ _ _      = error $ "Invalid instruction: " ++ show i
    in do
        instruction <- readArray arr ind
        if instruction == 99 
        then readArray arr 0
        else do
            xp     <- readArray arr (ind + 1)
            x      <- readArray arr xp
            yp     <- readArray arr (ind + 2)
            y      <- readArray arr yp
            target <- readArray arr (ind + 3)
            op instruction x y target
            performNext arr (ind + 4)
