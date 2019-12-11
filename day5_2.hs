import Data.Array.IO
import Data.Array.MArray
import Text.Show.Functions -- to prettyprint errors

type Address = Int
type Memory  = IOArray Int Int
type Value   = Int
type Input   = Int
data Opcode  = ADD | MUL | INP | OUT | JNZ | JZ | SLT | SEQ | HAL
    deriving Show

opcode 1  = ADD
opcode 2  = MUL
opcode 3  = INP
opcode 4  = OUT
opcode 5  = JNZ
opcode 6  = JZ
opcode 7  = SLT
opcode 8  = SEQ
opcode 99 = HAL
opcode e  = error $ "Unknown opcode " ++ (show e)

nParams ADD = 3 --writes on 3
nParams MUL = 3 --writes on 3
nParams INP = 1 --writes on 1
nParams OUT = 1 
nParams JNZ = 2 
nParams JZ  = 2 
nParams SLT = 3 --writes on 3
nParams SEQ = 3 --writes on 3
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
runInstruction :: Memory -> Address -> [Input] -> Opcode -> [Value] -> IO ([Input], Bool, Address) 
runInstruction mem ip is ADD vals = do 
    writeArray mem 
        (vals !! 2) 
        ((vals !! 0) + (vals !! 1))
    return (is, False, ip + 4)
runInstruction mem ip is MUL vals = do
    writeArray mem 
        (vals !! 2) 
        ((vals !! 0) * (vals !! 1))
    return (is, False, ip + 4)
runInstruction mem ip (i:is) INP vals = do
    writeArray mem (vals !! 0) i
    return (is, False, ip + 2) 
runInstruction _ _ _ INP _= error "Tried to read input; none found"
runInstruction mem ip is OUT vals = do
    putStrLn $ show $ vals !! 0
    return (is, False, ip + 2)
runInstruction mem ip is JNZ vals = 
    if (vals !! 0) /= 0
    then return (is, False, vals !! 1)
    else return (is, False, ip + 3)
runInstruction mem ip is JZ vals = 
    if (vals !! 0) == 0
    then return (is, False, vals !! 1)
    else return (is, False, ip + 3)
runInstruction mem ip is SLT vals = do
    if (vals !! 0) < (vals !! 1)
    then writeArray mem (vals !! 2) 1
    else writeArray mem (vals !! 2) 0
    return (is, False, ip + 4)
runInstruction mem ip is SEQ vals = do
    if (vals !! 0) == (vals !! 1)
    then writeArray mem (vals !! 2) 1
    else writeArray mem (vals !! 2) 0
    return (is, False, ip + 4)
runInstruction _ _ is HAL _ = return (is, True, error "Forced ip on halted machine")

--change the ops used in writes to immediate mode to defer lookup
demoteWrites :: Opcode -> [(Mode, Int)] -> [(Mode, Int)]
demoteWrites ADD (a:b:(m, i):xs) = a:b:(demote m, i):xs 
demoteWrites MUL (a:b:(m, i):xs) = a:b:(demote m, i):xs 
demoteWrites INP ((m,i):xs) = (demote m, i):xs
demoteWrites SEQ (a:b:(m, i):xs) = a:b:(demote m, i):xs 
demoteWrites SLT (a:b:(m, i):xs) = a:b:(demote m, i):xs 
demoteWrites _ l = l

demote POS = IMM
demote _   = error "Failed to demote mode"

performNext :: Memory -> Address -> [Input] -> IO Int
performNext mem ip input = do
    (op, ps, argms) <- getInstruction mem ip
    argms' <- return $ demoteWrites op argms
    putStrLn $ "DEBUG :" ++ (show (ip, op, ps, argms, argms'))
    vals <- sequence $ (unpackArg mem) <$> argms'
    putStrLn $ "DEBUG vals:" ++ (show vals)
    (input', halted, ip') <- runInstruction mem ip input op vals
    if halted
    then 
        readArray mem 0
    else
        performNext mem ip' input'

main = do
    f  <- readFile "day5.txt"
    --f  <- return "3,3,1107,-1,8,3,4,3,99"
    w  <- return $ (read :: String -> Int) <$> (words $ (\x -> if x == ',' then ' ' else x) <$> f)
    a  <- newListArray (0, length w - 1) w :: IO (IOArray Int Int)
    --i  <- readFile "day5_1input.txt" 
    --i' <- return $ (read :: String -> Int) <$> (words $ (\x -> if x == ',' then ' ' else x) <$> f)
    i' <- return [5]
    res <- performNext a 0 i'
    putStrLn $ "Halted: " ++ (show res)

