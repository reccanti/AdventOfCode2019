import System.Environment
import Text.Read
import Data.Typeable

-- Get the necessary fuel given a specified mass
calculateFuel :: (RealFrac a, Fractional a) => a -> Int
calculateFuel mass =
    floor (mass / 3) - 2

main = do
    args <- getArgs
    -- convert each argument into a number and then get it's fuel requirements
    -- then, get the sum of all of them
    let argsAsNums = [ read x | x <- args]
    let individualFuelReqs = [ calculateFuel x | x <- argsAsNums ]
    let totalFuel = sum individualFuelReqs
    print $ show $ totalFuel
    