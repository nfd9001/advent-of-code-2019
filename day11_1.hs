{-# LANGUAGE TupleSections #-}
import Data.List
import Data.Array.IO
import Data.Array.MArray
import qualified Data.Map.Strict as Map
import Control.Monad
import Data.IORef
import Text.Show.Functions -- to prettyprint errors

type Address = Integer
type Memory  = IOArray Integer Integer
type Value   = Integer
type Input   = Integer
type Output  = Integer
data Opcode  = ADD | MUL | INP | OUT | JNZ | JZ | SLT | SEQ | ORB | HAL
    deriving Show

opcode 1  = ADD
opcode 2  = MUL
opcode 3  = INP
opcode 4  = OUT
opcode 5  = JNZ
opcode 6  = JZ
opcode 7  = SLT
opcode 8  = SEQ
opcode 9  = ORB
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
nParams ORB = 1
nParams HAL = 0
    
data Mode = POS | IMM | RBP
    deriving Show
mode 0 = POS
mode 1 = IMM
mode 2 = RBP
mode e = error $ "Unknown mode " ++ show e

takeLast n = reverse . take n . reverse

--assumes infinite args; just use an appropriate bound
getModes :: Integer -> [Mode]
getModes l =
  let j = show l
  in reverse (mode . read . (: []) <$> take (length j - 2) j)
        ++ repeat POS

getOpcode :: Integer -> Opcode
getOpcode = opcode . (flip rem) 100
getInstruction :: Memory -> Address -> IO (Opcode, Integer, [(Mode, Integer)])
getInstruction mem a = do
  val  <- readArray mem a
  let op   = getOpcode val
  let ps   = nParams op
  let m    = getModes val
  args <- forwardArraySlice mem (a + 1) (a + ps + 1)
  return (op, ps, zip m args)

forwardArraySlice arr i j = sequence $ readArray arr <$> [i .. (j - 1)]

offsetIfNeeded :: Address -> (Mode, Integer) -> (Mode, Integer)
offsetIfNeeded rp (RBP, i) = (POS, i + rp)
offsetIfNeeded _ x = x

-- take a mode and an argument, and yield the value
unpackArg :: Memory -> Address -> (Mode, Integer) -> IO Value
unpackArg mem _  (POS, i) = readArray mem i
unpackArg _   _  (IMM, i) = return i
--currently unneeded
unpackArg _ _ _ = error $ "Unimplemented mode or tried to unpack RBP before offsetting" 

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

runInstruction mem ip rp i o ORB vals = 
  return (i, o, False, False, ip + 2, rp + (head vals))

runInstruction _ _ _ i o HAL _ =
  return (i, o, True, False, 
    error "Forced ip on halted machine", error "Forced rp on halted machine")

--change the ops used in writes to immediate mode to defer lookup
demoteWrites :: Opcode -> [(Mode, Integer)] -> [(Mode, Integer)]
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
  (op, ps, argms) <- getInstruction mem ip
  let offsetArgms = (offsetIfNeeded rp) <$> argms
  let demotedArgms = demoteWrites op offsetArgms 
  --putStrLn $ "DEBUG :" ++ (show (ip, op, ps, argms, argms'))
  vals <- sequence $ unpackArg mem rp <$> demotedArgms 
  --putStrLn $ "DEBUG vals:" ++ (show vals)
  (input', output', halted, blocked, ip', rp') <- runInstruction mem ip rp input output op vals
  --dump <- getAssocs mem
  --putStrLn $ "DEBUG: Dump" ++ (show dump)
  --if blocked then putStrLn "DEBUG: Machine blocked; switching context" else return ()
  if halted || blocked 
  then return (input', output', halted, blocked, ip', rp') 
  else performNext mem ip' rp' input' output'


-- a Machine in rest is always halted or blocked on input
data Machine = H HaltMachine | B BlockedMachine 
data HaltMachine = HaltMachine { 
    hmemory :: Memory,
    hinput :: [Integer],
    houtput :: [Integer]}

data BlockedMachine = BlockedMachine {
    bmemory :: Memory,
    ipreg :: Integer,
    rbreg :: Integer, -- needed for day 9
    boutput :: [Integer]}

newMachine :: Memory -> Machine
newMachine m = B (BlockedMachine m 0 0 [])

getMemory (B x) = bmemory x
getMemory (H x) = hmemory x
getOutput (B x) = boutput x
getOutput (H x) = houtput x
flushOutput (B x) = B $ x {boutput = []}
flushOutput (H x) = H $ x {houtput = []}
isHalted  (H _) = True
isHalted  _     = False

copyMemToNewMachine :: Memory -> IO Machine
copyMemToNewMachine m = do
    copy <- copyProgram m
    return $ newMachine copy 

giveMachineInput :: Machine -> [Integer] -> IO Machine
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
     
copyProgram :: Memory -> IO Memory
copyProgram = mapArray id
--thanks to jle` on #haskell on freenode
sortOnA f = fmap (map fst . sortOn snd) . traverse (\x -> (x,) <$> f x)

-- begin d11-specific code
data Direction = DUp | DDown | DLeft | DRight
leftturn DUp = DLeft 
leftturn DLeft = DDown 
leftturn DDown = DRight
leftturn DRight = DUp

rightturn DUp = DRight
rightturn DRight = DDown
rightturn DDown = DLeft
rightturn DLeft = DUp

step DUp (x, y) = (x, y+1)
step DDown (x, y) = (x, y-1)
step DLeft (x, y) = (x-1, y)
step DRight (x, y) = (x+1, y)

robotStep map pos dir machine = do 
    let curcol = Map.findWithDefault 0 pos map
    machine' <- giveMachineInput machine [curcol]
    let (newcol:turndir:_) = getOutput machine'    
    let map' = Map.insert pos newcol map
    let dir' = if turndir == 0 then leftturn dir else rightturn dir
    let pos' = step dir' pos
    --print $ getOutput machine'
    --putStrLn $ "DEBUG: turndir is " ++ (show turndir)
    --putStrLn $ "DEBUG: moving from " ++ (show pos) ++ " to " ++ (show pos')
    if isHalted machine'
    then return map'
    else robotStep map' pos' dir' (flushOutput machine')

main = do
    f <- readFile "day11.txt"
    let w =  (read :: String -> Integer) <$> words ((\x -> if x == ',' then ' ' else x) <$> f)
    a  <- newListArray (0, 10000) (w ++ (repeat 0)) :: IO (IOArray Integer Integer)
    m <- return $ newMachine a
    map <- robotStep Map.empty (0,0) DUp m
    print $ Map.size map
    --print $ Map.toList map

