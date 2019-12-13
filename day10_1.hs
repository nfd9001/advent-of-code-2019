import Control.Monad
import Data.List
import qualified Data.Set as Set

angle (x1, y1) (x2,y2) = atan2 (fromIntegral $ y2 - y1) (fromIntegral $ x2 - x1)

main = do
    f <- readFile "day10.txt"
    let nls = zip [0..] $ fmap (\x -> fst x) . filter (\x -> snd x == '#') . zip [0..] <$> lines f
    let g (x,ys) = do y <- ys; return (x, y)
    let pairs = join $ g <$> nls
    
    let nDistinctAngles = do
        pair <- pairs
        let rest = delete pair pairs
        return $ (Set.size $ Set.fromList $ angle pair <$> rest, pair)
        
    --reused this to get the correct location with which to start the next step
    --at first, I didn't preserve the pairs, and I just output the sizes
    --a bug that then snuck up on me was that my coordinate system is actually
    --backwards...
    print $ last $ sortOn fst nDistinctAngles 
