{-# LANGUAGE TupleSections #-}
import Data.List
import Data.Array.IO
import Data.Array.MArray
import Text.Show.Functions -- to prettyprint errors

type Address = Int
type Memory  = IOArray Int Int
type Value   = Int
type Input   = Int
type Output  = Int
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
opcode e  = error $ "Unknown opcode " ++ show e

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
mode e = error $ "Unknown mode " ++ show e

takeLast n = reverse . take n . reverse

--assumes infinite args; just use an appropriate bound
getModes :: Int -> [Mode]
getModes l =
  let j = show l
  in reverse (mode . read . (: []) <$> take (length j - 2) j)
        ++ repeat POS

getOpcode :: Int -> Opcode
getOpcode = opcode . read . takeLast 2 . show

getInstruction :: Memory -> Address -> IO (Opcode, Int, [(Mode, Int)])
getInstruction mem a = do
  val  <- readArray mem a
  let op   = getOpcode val
  let ps   = nParams op
  let m    = getModes val
  args <- forwardArraySlice mem (a + 1) (a + ps + 1)
  return (op, ps, zip m args)

forwardArraySlice arr i j = sequence $ readArray arr <$> [i .. (j - 1)]

-- take a mode and an argument, and yield the value
unpackArg :: Memory -> (Mode, Int) -> IO Value
unpackArg mem (POS, i) = readArray mem i
unpackArg _   (IMM, i) = return i
--currently unneeded
--unpackArg _   (e  , _) = error $ "Unimplemented mode " ++ (show e)

-- (remaining inputs, halted?)
runInstruction
  :: Memory
  -> Address
  -> [Input]
  -> [Output]
  -> Opcode
  -> [Value]
  -> IO ([Input], [Output], Bool, Address)
runInstruction mem ip i o ADD vals = do
  writeArray mem (vals !! 2) ((vals !! 0) + (vals !! 1))
  return (i, o, False, ip + 4)
runInstruction mem ip i o MUL vals = do
  writeArray mem (vals !! 2) ((vals !! 0) * (vals !! 1))
  return (i, o, False, ip + 4)
runInstruction mem ip (i : is) o INP vals = do
  writeArray mem (vals !! 0) i
  -- putStrLn $ "DEBUG Machine read " ++ show i
  return (is, o, False, ip + 2)
runInstruction _   _  _ _ INP _    = error "Tried to read input; none found"
runInstruction mem ip i o OUT vals = do
  q <- return $ head vals
  -- putStrLn $ "DEBUG Machine output " ++ show q 
  let o' = o ++ [q]
  return (i, o', False, ip + 2)
runInstruction mem ip i o JNZ vals = if (vals !! 0) /= 0
  then return (i, o, False, vals !! 1)
  else return (i, o, False, ip + 3)
runInstruction mem ip i o JZ vals = if (vals !! 0) == 0
  then return (i, o, False, vals !! 1)
  else return (i, o, False, ip + 3)
runInstruction mem ip i o SLT vals = do
  if (vals !! 0) < (vals !! 1)
    then writeArray mem (vals !! 2) 1
    else writeArray mem (vals !! 2) 0
  return (i, o, False, ip + 4)
runInstruction mem ip i o SEQ vals = do
  if (vals !! 0) == (vals !! 1)
    then writeArray mem (vals !! 2) 1
    else writeArray mem (vals !! 2) 0
  return (i, o, False, ip + 4)
runInstruction _ _ i o HAL _ =
  return (i, o, True, error "Forced ip on halted machine")

--change the ops used in writes to immediate mode to defer lookup
demoteWrites :: Opcode -> [(Mode, Int)] -> [(Mode, Int)]
demoteWrites ADD (a : b : (m, i) : xs) = a : b : (demote m, i) : xs
demoteWrites MUL (a : b : (m, i) : xs) = a : b : (demote m, i) : xs
demoteWrites INP ((m, i)         : xs) = (demote m, i) : xs
demoteWrites SEQ (a : b : (m, i) : xs) = a : b : (demote m, i) : xs
demoteWrites SLT (a : b : (m, i) : xs) = a : b : (demote m, i) : xs
demoteWrites _   l                     = l

demote POS = IMM
demote _   = error "Failed to demote mode"

performNext :: Memory -> Address -> [Input] -> [Output] -> IO [Int]
performNext mem ip input output = do
  (op, ps, argms)       <- getInstruction mem ip
  let argms' = demoteWrites op argms
  --putStrLn $ "DEBUG :" ++ (show (ip, op, ps, argms, argms'))
  vals                  <- sequence $ unpackArg mem <$> argms'
  --putStrLn $ "DEBUG vals:" ++ (show vals)
  (input', output', halted, ip') <- runInstruction mem ip input output op vals
  if halted then return output' else performNext mem ip' input' output'

copyProgram = mapArray id

runAmplifier :: Memory -> [Int] -> IO [Int] -> IO [Int]
runAmplifier machine phase last = do 
  copy <- copyProgram machine
  l   <- last
  --putStrLn $ "DEBUG Running machine phase" ++ show p ++ " last " ++ show last 
  performNext copy 0 (phase ++ l) []

runAllAmplifiers :: Memory -> [[Int]] -> IO Int
runAllAmplifiers machine =  fmap head . foldr (runAmplifier machine) (return [0])

--thanks to jle` on #haskell on freenode
sortOnA f = fmap (map fst . sortOn snd) . traverse (\x -> (x,) <$> f x)

main = do
  f <- readFile "day7.txt"
  --f  <- return "3,3,1107,-1,8,3,4,3,99"
  let w =  (read :: String -> Int) <$> words ((\x -> if x == ',' then ' ' else x) <$> f)
  a  <- newListArray (0, length w - 1) w :: IO (IOArray Int Int)
  --because inputs are lists (like streams), wrap 'em
  let phases = permutations $ (: []) <$> [0..4]

  -- this does compute the correct value twice, but doing this this way made
  -- debugging somewhat easier
  res' <- sortOnA (runAllAmplifiers a) phases
  res <- runAllAmplifiers a (last res') 
  print res
