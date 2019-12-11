import Data.Array.IO
import Data.Array.MArray
import Text.Show.Functions -- to prettyprint errors

type Address = Int
type Memory  = IOArray Int Int
type Value   = Int
type Input   = Int
data Opcode  = ADD | MUL | INP | OUT | HAL
    deriving Show

opcode 1  = ADD
opcode 2  = MUL
opcode 3  = INP
opcode 4  = OUT
opcode 99 = HAL
opcode e  = error $ "Unknown opcode " ++ (show e)

nParams ADD = 3
nParams MUL = 3
nParams INP = 1
nParams OUT = 1
nParams HAL = 0

data Mode = POS | IMM
    deriving Show
mode 0 = POS
mode 1 = IMM
mode e = error $ "Unknown mode " ++ (show e)

takeLast n = reverse . take n . reverse 

--assumes infinite args; just use an appropriate bound
getModes :: Int -> [Mode]
getModes l = let j = show l
    in reverse ((mode . read . (\x -> [x])) <$>
    take ((length j) - 2) j) ++ (repeat POS)
      
getOpcode :: Int -> Opcode 
getOpcode = opcode . read . takeLast 2 . show

getInstruction :: Memory -> Address -> IO (Opcode, Int, [(Mode, Int)])
getInstruction mem a = do
    val  <- readArray mem a
    op   <- return $ getOpcode val
    ps   <- return $ nParams op
    m    <- return $ getModes val
    args <- forwardArraySlice mem (a + 1) (a + ps + 1) 
    return (op, ps, zip m args)

forwardArraySlice arr i j = sequence $ readArray arr <$> [i..(j-1)]

-- take a mode and an argument, and yield the value
unpackArg :: Memory -> (Mode, Int) -> IO Value 
unpackArg mem (POS, i) = readArray mem i
unpackArg _   (IMM, i) = return i
--currently unneeded
--unpackArg _   (e  , _) = error $ "Unimplemented mode " ++ (show e)

-- (remaining inputs, halted?)
runInstruction :: Memory -> [Input] -> Opcode -> [Value] -> IO ([Input], Bool) 
runInstruction mem is ADD vals = do 
    writeArray mem 
        (vals !! 2) 
        ((vals !! 0) + (vals !! 1))
    return (is, False)
runInstruction mem is MUL vals = do
    writeArray mem 
        (vals !! 2) 
        ((vals !! 0) * (vals !! 1))
    return (is, False)
runInstruction mem (i:is) INP vals = do
    writeArray mem (vals !! 0) i
    return (is, False) 
runInstruction _ _ INP _ = error "Tried to read input; none found"
runInstruction mem is OUT vals = do
    putStrLn $ show $ vals !! 0
    return (is, False)
runInstruction _ is HAL _ = return (is, True)

--change the ops used in writes to immediate mode to defer lookup
filterWrites :: Opcode -> [(Mode, Int)] -> [(Mode, Int)]
filterWrites ADD (a:b:(_, i):xs) = a:b:(IMM,i):xs 
filterWrites MUL (a:b:(_, i):xs) = a:b:(IMM,i):xs 
filterWrites INP ((_,i):xs) = (IMM,i):xs
filterWrites _ l = l

performNext :: Memory -> Address -> [Input] -> IO Int
performNext mem ip input = do
    (op, ps, argms) <- getInstruction mem ip
    argms' <- return $ filterWrites op argms
    --putStrLn $ "DEBUG :" ++ (show (ip, op, ps, argms))
    vals <- sequence $ (unpackArg mem) <$> argms'
    --putStrLn $ "DEBUG vals:" ++ (show (ip, op, ps, argms, vals))
    (input', halted) <- runInstruction mem input op vals
    if halted
    then do
        readArray mem 0
    else
        performNext mem (ip + ps + 1) input'

main = do
    f  <- readFile "day5.txt"
    w  <- return $ (read :: String -> Int) <$> (words $ (\x -> if x == ',' then ' ' else x) <$> f)
    a  <- newListArray (0, length w - 1) w :: IO (IOArray Int Int)
    --i  <- readFile "day5_1input.txt" 
    --i' <- return $ (read :: String -> Int) <$> (words $ (\x -> if x == ',' then ' ' else x) <$> f)
    i' <- return [1]
    res <- performNext a 0 i'
    putStrLn $ "Halted: " ++ (show res)


