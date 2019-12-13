{-# LANGUAGE TupleSections #-}
import Control.Monad 
import Data.List
import Data.Tuple
import qualified Data.Set as Set

angle (x1, y1) (x2, y2) = let 
    --yes, really x then y
    a = atan2 (fromIntegral $ x2 - x1) (-(fromIntegral $ y2 - y1))
    in if a < 0 then 2 * pi + a else a

distance (x1, y1) (x2, y2) = sqrt((fromIntegral $ x1 - x2)**2 + (fromIntegral $ y1 - y2)**2)
interleave = concat . transpose
    
main = do
    f <- readFile "day10.txt"
    let nls = zip [0..] $ fmap (\x -> fst x) . filter (\x -> snd x == '#') . zip [0..] <$> lines f
    let g (x,ys) = do y <- ys; return (x, y)
    let pairs = swap <$> (join $ g <$> nls :: [(Integer, Integer)])
    let best = (20, 21) 
    --let best = (11,13) 
    let rest = delete best pairs
    
    let i = groupBy (\x y -> snd x == snd y) $ sortOn snd $(\x -> (x, angle best x)) <$> rest
    let j = do
        l <- i
        return $ sortOn (distance best . fst) l
    let a = interleave j 
    print a
    putStrLn $ "1st " ++ (show $ a!!0)
    putStrLn $ "2nd " ++ (show $ a!!1)
    putStrLn $ "3nd " ++ (show $ a!!2)
    putStrLn $ "10th " ++ (show $ a!!9)
    putStrLn $ "20th " ++ (show $ a!!19)
    putStrLn $ "30th " ++ (show $ a!!29)
    putStrLn $ "50th " ++ (show $ a!!49)
    putStrLn $ "100th " ++ (show $ a!!99)
    putStrLn $ "199th " ++ (show $ a!!198)
    putStrLn $ "200th " ++ (show $ a!!199)
    putStrLn $ "201st " ++ (show $ a!!200)
    putStrLn $ "299th " ++ (show $ a!!298)
