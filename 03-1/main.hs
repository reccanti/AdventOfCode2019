import System.Environment
import System.IO
import Text.Read
import Data.List.Split

data Direction = North Int | South Int | West Int | East Int deriving Show

data Bounds = Bounds {
    left :: Int,
    bottom :: Int,
    right :: Int,
    top  :: Int 
} deriving Show

initBounds = Bounds {
    left = 0,
    bottom = 0,
    right = 0,
    top = 0
}

data Coordinates = Coordinates {
    x :: Int,
    y :: Int
} deriving Show

initCoordinates = Coordinates {
    x = 0,
    y = 0
}

type Line = (Coordinate, Coordinate)

updateBounds :: Bounds -> Coordinates -> Direction -> Bounds
updateBounds b@(Bounds { left=left }) (Coordinates { x=curX }) (West x)
    | netX < left = b { left=netX }
    | otherwise = b
    where netX = curX-x
updateBounds b@(Bounds { right=right }) (Coordinates { x=curX }) (East x)
    | netX > right = b { right=netX }
    | otherwise = b
    where netX = curX + x
updateBounds b@(Bounds { bottom=bottom }) (Coordinates { y=curY }) (South y)
    | netY < bottom = b { bottom=netY }
    | otherwise = b
    where netY = curY - y
updateBounds b@(Bounds { top=top }) (Coordinates { y=curY }) (North y)
    | netY > top = b { top=netY }
    | otherwise = b
    where netY = curY + y

updateCoordinates :: Coordinates -> Direction -> Coordinates
updateCoordinates coordinates@(Coordinates { x=curX }) (West x) =
    coordinates { x=(curX - x) }
updateCoordinates coordinates@(Coordinates { x=curX }) (East x) =
    coordinates { x=(curX + x) }
updateCoordinates coordinates@(Coordinates { y=curY }) (South y) =
    coordinates { y=(curY - y) }
updateCoordinates coordinates@(Coordinates { y=curY }) (North y) =
    coordinates { y=(curY + y) }

-- Given a series of directions, determine the bounds
-- of the field
getBounds ::  [Direction] -> Bounds
getBounds directions = bounds
    where 
        acc = (initCoordinates, initBounds)
        update (coords, bounds) direction =
            (updateCoordinates coords direction, updateBounds bounds coords direction)
        (_, bounds) = foldl update acc directions

-- Convenience function to print the bounds
printBounds :: Bounds -> String
printBounds (Bounds { left=left, bottom=bottom, top=top, right=right }) = 
    unlines $ take (top - bottom) $ repeat writeRow
    where writeRow = take (right - left) $ repeat '.'

-- a function which parses input into Directions
parseWire ::  [String] -> [Direction]
parseWire strings = 
    [ parse x | x <- strings]
    where 
        parse ('L':ints) = West  (read ints :: Int)
        parse ('R':ints) = East  (read ints :: Int)
        parse ('D':ints) = South (read ints :: Int)
        parse ('U':ints) = North (read ints :: Int)

main = do
    (filename:_) <- getArgs
    contents <- readFile filename
    let wireStrings = filter (\wire -> wire /= "") $ splitOn "\n" contents
    let wireDirections = map (\string -> splitOn "," string) wireStrings
    let wires = [ parseWire wire | wire <- wireDirections ]
    print $ getBounds $ head $ drop 1 wires

-- drawWires :: [Direction] -> String
