import System.Environment
import Text.Read
import Data.List.Split

-- The Intcode State consists of the current
-- position of the interpreter in the Intcode
-- list, and the current Intcode list
type Intcodes = [Int]
type IntcodeState = (Int, Intcodes)

-- a utility function to replace a value at a
-- given index in a list
replaceAt :: Int -> a -> [a] -> [a]
replaceAt index val (x:xs)
    | index == 0 = val:xs
    | otherwise  = x:(replaceAt (index - 1) val xs)

-- a utility function to get the value at a
-- given index from a list
getAt :: Int -> [a] -> a
getAt index (x:xs)
    | index == 0 = x
    | otherwise  = getAt (index - 1) xs

-- The state works by traversing a list of Intcodes
-- The value returned is the current set of Intcodes
-- to work on, and the state is the current configuration
-- of Intcodes
parseCode :: IntcodeState -> IntcodeState
parseCode state@(startingIndex, intcodes) 
    | opcode == 1  = handleAdd state
    | opcode == 2  = handleMultiply state
    | opcode == 99 = handleStop state
    | otherwise    = handleContinue state
    where opcode:_ = drop startingIndex intcodes

-- Given the current state, add the values at
-- the given indices and store them at the given
-- position. Afterwards, update the state
handleAdd :: IntcodeState -> IntcodeState
handleAdd (startingIndex, intcodes) = 
    let 
        (_, (opcode:val1Index:val2Index:storeIndex:_)) = splitAt startingIndex intcodes
        val1 = getAt val1Index intcodes
        val2 = getAt val2Index intcodes
    in
        (startingIndex + 4, replaceAt storeIndex (val1 + val2) intcodes)

-- Given the current state, multiply the values at
-- the given indices and store them at the given
-- position. Afterwards, update the state
handleMultiply :: IntcodeState -> IntcodeState
handleMultiply (startingIndex, intcodes) = 
    let 
        (_, (opcode:val1Index:val2Index:storeIndex:_)) = splitAt startingIndex intcodes
        val1 = getAt val1Index intcodes
        val2 = getAt val2Index intcodes
    in
        (startingIndex + 4, replaceAt storeIndex (val1 * val2) intcodes)

-- Given the current state, set the index to -1 and
-- return the current state. In the future, it might be
-- good to replace this with some sort of data type instead
-- of relying on a code like -1
handleStop :: IntcodeState -> IntcodeState
handleStop (startingIndex, intcodes) = (-1, intcodes)

-- Given the current state, just iterate to the next line
-- as if nothing happened
handleContinue :: IntcodeState -> IntcodeState
handleContinue state = state

-- given an initial state continuously iterate
-- through the list until a given condition is met
parseLoop :: IntcodeState -> IntcodeState
parseLoop state@(-1, intcodes) = state
parseLoop state@(startingIndex, intcodes) = parseLoop $ parseCode state

main = do
    args <- getArgs
    -- split the arguments by commas, then parse them into
    -- a list of ints
    let initialState = [read x :: Int | x <- splitOn "," $ head args]
    -- update to state before 1202 program alarm by 
    -- replacing position 1 with 12 and postion 2 with 2
    let preErrorState = replaceAt 2 2 $ replaceAt 1 12 initialState
    let finalState = parseLoop (0, preErrorState)
    let (_, finalAnswer:_) = finalState
    print finalAnswer
