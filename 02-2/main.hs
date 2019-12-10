import System.Environment
import Text.Read
import Data.List.Split

-- a data type to represent the Progress through the
-- Intcode machine
data Progress a = Continue a | Halt deriving (Show)

-- a data type representing the current state of the
-- Intcode machine
type State a = [a]

getVal :: Int -> State a -> Maybe a
getVal index state
    | index >= length state = Nothing
    | index == 0 = Just x
    | otherwise = getVal (index - 1) xs
    where x:xs = state

-- update the value in the state. If the index is 
-- out of bounds, nothing will happen
updateVal :: Int -> a -> State a -> State a
updateVal index value state@(x:xs)
    | index >= length state = state
    | index == 0 = value:xs
    | otherwise = x:(updateVal (index - 1) value xs)

-- a data type representing the total Intcode machine
type IntcodeMachine = (Progress Int, State Int)

-- a function which initializes an Intcode machine
-- from an initial state
initMachine :: State Int -> IntcodeMachine
initMachine initialState = (Continue 0, initialState)

process :: IntcodeMachine -> IntcodeMachine
process m@(Halt, _) = m
process m@(Continue index, state) 
    | index >= length state = (Halt, state)
    | otherwise = case opcode of
        Nothing -> (Halt, state)
        Just code -> processCode code
    where
        opcode = getVal index state
        processCode code
            | code == 1 = opAdd m
            | code == 2 = opMult m
            | otherwise = (Halt, state)

opAdd :: IntcodeMachine -> IntcodeMachine
opAdd m@(Halt, _) = m
opAdd m@(Continue index, state) = 
    (Continue (index + 4), updatedState)
    where
        (_, opcode:val1index:val2index:storeIndex:_) = splitAt index state
        val1 = getVal val1index state
        val2 = getVal val2index state
        updatedState = case (val1, val2) of
            (Just v1, Just v2) -> updateVal storeIndex (v1 + v2) state
            otherwise -> state

opMult :: IntcodeMachine -> IntcodeMachine
opMult m@(Halt, _) = m
opMult m@(Continue index, state) = 
    (Continue (index + 4), updatedState)
    where
        (_, opcode:val1index:val2index:storeIndex:_) = splitAt index state
        val1 = getVal val1index state
        val2 = getVal val2index state
        updatedState = case (val1, val2) of
            (Just v1, Just v2) -> updateVal storeIndex (v1 * v2) state
            otherwise -> state
            
runMachine :: IntcodeMachine -> IntcodeMachine
runMachine m@(Halt, _) = m
runMachine m@(Continue index, state) = runMachine $ process m

-- run the IntcodeMachine, but accept a "noun" and "verb"
-- initial starting values
runMachineWithArgs :: IntcodeMachine -> Int -> Int -> IntcodeMachine
runMachineWithArgs m@(progress, start:_:_:rest) name verb =
    let 
        initializedState = start:name:verb:rest 
        initializedMachine = initMachine initializedState
     in
        runMachine initializedMachine

-- Given a target value and an IntcodeMachine, 
-- find the noun/verb pair that produces the desired
-- result
-- getInputs :: Int -> State Int -> Maybe (Int, Int)
-- getInputs target state 
--     | result == target = Just (noun, verb)
--     | otherwise 
--         | noun == 99 && verb == 99 = Nothing
--         | noun == 99 = runMachineWithArgs m 0 (verb + 1)
--         | otherwise = runMachineWithArgs m (noun + 1) verb
--     where
--         m = initMachine state
--         noun = 0
--         verb = 0
getInputs :: State Int -> Int ->  [(Int, Int)] -> Maybe (Int, Int)
getInputs initialState target potentialInputs = 
    case potentialInputs of 
        [] -> Nothing
        _ -> 
            if result == target then Just cur
            else getInputs initialState target rest
    where
        (cur:rest) = potentialInputs
        (noun, verb) = cur
        m = runMachineWithArgs (initMachine initialState) noun verb
        (_, result:_) = m



main = do
    (stateString:target:_) <- getArgs
    -- split the arguments by commas, then parse them into
    -- a list of ints
    let initialState = [read x :: Int | x <- splitOn "," $ stateString]
    -- get the target value
    let targetValue = read target :: Int
    -- given a target number, get the input noun and verb
    let potentialInputs = [ (x, y) | x <- [0..99], y <- [0..99] ]
    let result = getInputs initialState targetValue potentialInputs
    case result of
        Just res -> print res
        Nothing -> print "Nothing"