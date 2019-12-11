import Data.Array.IO
import Data.Array.MArray
import System.Exit
main = do
    f <- readFile "day2.txt"
    w <- return $ (read :: String -> Int) <$> (words $ (\x -> if x == ',' then ' ' else x) <$> f)
    a <- newListArray (0, length w - 1) w :: IO (IOArray Int Int)
    pairs <- return [(noun, verb) | noun <- [1..99], verb <- [1..99]]
    sequence $ checkPair a 19690720 <$> pairs
    exitFailure

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

checkPair :: IOArray Int Int -> Int -> (Int, Int) -> IO ()
checkPair arr value (noun, verb) = do
    copy <- mapArray id arr
    writeArray copy 1 noun
    writeArray copy 2 verb
    res <- performNext copy 0
    if res == value then (putStrLn $ show (100 * noun + verb)) >>= \_ -> exitSuccess else return ()
