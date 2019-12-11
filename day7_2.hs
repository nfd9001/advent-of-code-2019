{-# LANGUAGE TupleSections #-}
import Data.List
import Data.Array.IO
import Data.Array.MArray
import Control.Monad
import Data.IORef
import Text.Show.Functions -- to prettyprint errors

type Address = Int
type Memory  = IOArray Int Int
type Value   = Int
type Input   = Int
type Output  = Int
data Opcode  = ADD | MUL | INP | OUT | JNZ | JZ | SLT | SEQ | HAL
    deriving Show

-- a Machine in rest is always halted or blocked on input
data Machine = H HaltMachine | B BlockedMachine 
data HaltMachine = HaltMachine { 
    hmemory :: Memory,
    hinput :: [Int],
    houtput :: [Int]}

data BlockedMachine = BlockedMachine {
    bmemory :: Memory,
    ipreg :: Int,
    rbreg :: Int, -- needed for day 9
    boutput :: [Int]}

newMachine :: Memory -> Machine
newMachine m = B (BlockedMachine m 0 0 [])

getMemory (B x) = bmemory x
getMemory (H x) = hmemory x

isHalted (H _) = True
isHalted _     = False

copyMemToNewMachine :: Memory -> IO Machine
copyMemToNewMachine m = do
    copy <- copyProgram m
    return $ newMachine copy 

giveMachineInput :: Machine -> [Int] -> IO Machine
giveMachineInput (H x) l = return $ H (x {hinput = (hinput x) ++ l})
giveMachineInput (B x) l = 
    if null l 
    then error "You probably didn't mean to pass a halted machine no input."
    else do
        let m = bmemory x
        let ip = ipreg x
        let rp = rbreg x
        let o  = boutput x
        (input', output', halted, blocked, ip', rp') <-
            performNext m ip rp l o
        if halted
        then return $ H $ HaltMachine m input' output'
        else if blocked
            then return $ B $ BlockedMachine m ip' rp' output'
            else error "Got an unblocked, unhalted instruction response"
     
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
getOpcode = opcode . (flip rem) 100
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
  -> Address
  -> [Input]
  -> [Output]
  -> Opcode
  -> [Value]
  -> IO ([Input], [Output], Bool, Bool, Address, Address) --bool 0 is halt, 1 is block
runInstruction mem ip rp i o ADD vals = do
  writeArray mem (vals !! 2) ((vals !! 0) + (vals !! 1))
  return (i, o, False, False, ip + 4, rp)

runInstruction mem ip rp i o MUL vals = do
  writeArray mem (vals !! 2) ((vals !! 0) * (vals !! 1))
  return (i, o, False, False, ip + 4, rp)

runInstruction mem ip rp (i : is) o INP vals = do
  writeArray mem (vals !! 0) i
  -- putStrLn $ "DEBUG Machine read " ++ show i
  return (is, o, False, False, ip + 2, rp)
runInstruction mem ip rp i o INP vals = return (i, o, False, True, ip, rp)
--runInstruction _  _  _ _ INP _    = error "Tried to read input; none found"

runInstruction mem ip rp i o OUT vals = do
  q <- return $ head vals
  -- putStrLn $ "DEBUG Machine output " ++ show q 
  let o' = o ++ [q]
  return (i, o', False, False, ip + 2, rp)

runInstruction mem ip rp i o JNZ vals = if (vals !! 0) /= 0
  then return (i, o, False, False, vals !! 1, rp)
  else return (i, o, False, False, ip + 3, rp)

runInstruction mem ip rp i o JZ vals = if (vals !! 0) == 0
  then return (i, o, False, False, vals !! 1, rp)
  else return (i, o, False, False, ip + 3, rp)

runInstruction mem ip rp i o SLT vals = do
  if (vals !! 0) < (vals !! 1)
    then writeArray mem (vals !! 2) 1
    else writeArray mem (vals !! 2) 0
  return (i, o, False, False, ip + 4, rp)

runInstruction mem ip rp i o SEQ vals = do
  if (vals !! 0) == (vals !! 1)
    then writeArray mem (vals !! 2) 1
    else writeArray mem (vals !! 2) 0
  return (i, o, False, False, ip + 4, rp)

runInstruction _ _ _ i o HAL _ =
  return (i, o, True, False, 
    error "Forced ip on halted machine", error "Forced rp on halted machine")

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

performNext :: Memory -> 
    Address -> 
    Address ->
    [Input] ->
    [Output] ->
    IO ([Input], [Output], Bool, Bool, Address, Address)
performNext mem ip rp input output = do
  (op, ps, argms)       <- getInstruction mem ip
  let argms' = demoteWrites op argms
  --putStrLn $ "DEBUG :" ++ (show (ip, op, ps, argms, argms'))
  vals                  <- sequence $ unpackArg mem <$> argms'
  --putStrLn $ "DEBUG vals:" ++ (show vals)
  (input', output', halted, blocked, ip', rp') <- runInstruction mem ip rp input output op vals
  --dump <- getAssocs mem
  --putStrLn $ "DEBUG: Dump" ++ (show dump)
  --if blocked then putStrLn "DEBUG: Machine blocked; switching context" else return ()
  if halted || blocked 
  then return (input', output', halted, blocked, ip', rp') 
  else performNext mem ip' rp' input' output'

copyProgram :: Memory -> IO Memory
copyProgram = mapArray id

runAllAmplifiersCycle :: Memory -> [[Input]] -> IO Output 
runAllAmplifiersCycle mem phases = do
    init <- sequence $
        (\x -> (do i <- copyMemToNewMachine mem; giveMachineInput i x)) <$>
         phases 
    ref <- sequence $ newIORef <$> init
    let machines = cycle ref
    --putStrLn $ "DEBUG: starting cycle with phases " ++ show phases
    runNextAmplifierCycle machines [0] 

runNextAmplifierCycle :: [IORef Machine] -> [Input] -> IO Output
runNextAmplifierCycle (m:ms) inp = let
    next (B x) ref ms = do
        writeIORef ref (B x {boutput = []}) 
        runNextAmplifierCycle ms (boutput x)
    next (H x) ref (m:ms) = do
        writeIORef ref (H x {houtput = []}) 
        m' <- readIORef m
        if isHalted m'
        then return $ last $ houtput x
        else runNextAmplifierCycle (m:ms) (houtput x)
 in do
    m' <- readIORef m
    m'' <- giveMachineInput m' inp
    next m'' m ms

--thanks to jle` on #haskell on freenode
sortOnA f = fmap (map fst . sortOn snd) . traverse (\x -> (x,) <$> f x)

main = do
  f <- readFile "day7.txt"
  --f  <- return "3,3,1107,-1,8,3,4,3,99"
  let w =  (read :: String -> Int) <$> words ((\x -> if x == ',' then ' ' else x) <$> f)
  a  <- newListArray (0, length w - 1) w :: IO (IOArray Int Int)
  --because inputs are lists (like streams), wrap 'em
  let phases = permutations $ (: []) <$> [5..9]
  -- this does compute the correct value twice, but doing this this way made
  -- debugging somewhat easier
  res' <- sortOnA (runAllAmplifiersCycle a) phases
  print res'
  res <- runAllAmplifiersCycle a (last res') 
  print $ join <$> res'
  print res
