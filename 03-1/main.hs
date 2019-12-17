import System.Environment
import System.IO
import Text.Read
import Data.List.Split
import Data.Maybe (catMaybes)

-- A convenience function that is similar to a map,
-- but with an accumulator that can be used to 
-- aggregate values based on previous items in the list
mapAccum :: (a -> b -> a) -> a -> [b] -> [a]
mapAccum _ _ [] = []
mapAccum f accum (x:xs) = 
    let newAccum = f accum x in
        newAccum : mapAccum f newAccum xs

-- a convenience function to return the smallest
-- item in a list
minElement :: [Int] -> Int
minElement [] = 0
minElement [x] = x
minElement (x:y:xs)
    | x > y = minElement (y:xs)
    | x < y = minElement (x:xs)
    | x == y = minElement (x:xs)

data Coordinate = Coordinate {
    x :: Int,
    y :: Int
} deriving Show

type Line = (Coordinate, Coordinate)

-- convenience function. Can make it easier to compare lines
reverseLine :: Line -> Line
reverseLine (c1, c2) = (c2, c1)

-- Coordinate Addition
addC :: Coordinate -> Coordinate -> Coordinate
addC (Coordinate x1 y1) (Coordinate x2 y2) = Coordinate (x1 + x2) (y1 + y2)

-- Coordinate Scalar Multiplication
sMult :: Int -> Coordinate -> Coordinate
sMult s (Coordinate x y) = Coordinate (s*x) (s*y) 

initCoordinate = Coordinate {
    x = 0,
    y = 0
}

-- a convenience function to parse the codes of the program
-- to a series of vectors that can be more easily added
codeToVector :: String -> Coordinate
codeToVector (dir:int) = 
    let coords = (dir, read int :: Int) in
        case coords of
            ('L', amount) -> Coordinate (-amount) 0
            ('R', amount) -> Coordinate amount 0
            ('D', amount) -> Coordinate 0 (-amount)
            ('U', amount) -> Coordinate 0 amount

-- given a list of coordinates, convert that into a 
-- series of coordinates
directionsToCoordinates :: [String] -> [Coordinate]
directionsToCoordinates directions = 
    mapAccum (\cur dir -> move cur dir) initCoordinate directions
    where
        move cur dir = addC cur (codeToVector dir)

-- given a list of coordinates, make a list of
-- sequential segments
coordinatesToSegments :: [Coordinate] -> [Line]
coordinatesToSegments start@(_:offset) = 
    zip start offset

-- a convenience function. Given 2 lines, determine if the
-- first line vertically overlaps the other
getVOverlap :: Line -> Line -> Maybe Int
getVOverlap l1 l2
    | y11 == y12 && ((y21 <= y11 && y22 >= y11) || (y21 >= y11 && y22 <= y11)) = Just y11
    | y21 == y22 && ((y11 <= y21 && y12 >= y21) || (y11 >= y21 && y12 <= y22)) = Just y21
    | otherwise = Nothing
    where
        ((Coordinate _ y11), (Coordinate _ y12)) = l1
        ((Coordinate _ y21), (Coordinate _ y22)) = l2

getHOverlap :: Line -> Line -> Maybe Int
getHOverlap l1 l2
    | x11 == x12 && ((x21 <= x11 && x22 >= x11) || (x21 >= x11 && x22 <= x11)) = Just x11
    | x21 == x22 && ((x11 <= x21 && x12 >= x21) || (x11 >= x21 && x12 <= x22)) = Just x21
    | otherwise = Nothing
    where
        ((Coordinate x11 _), (Coordinate x12 _)) = l1
        ((Coordinate x21 _), (Coordinate x22 _)) = l2

getSegmentIntersection :: Line -> Line -> Maybe Coordinate
getSegmentIntersection l1 l2 =
    case (hIntersection, vIntersection) of
        (Just x, Just y) -> Just $ Coordinate x y
        _ -> Nothing
    where
        hIntersection = getHOverlap l1 l2
        vIntersection = getVOverlap l1 l2


getAllIntersections :: [Line] -> [Line] -> [Coordinate]
getAllIntersections w1 w2 =
    catMaybes $ [ getSegmentIntersection s1 s2 | s1 <- w1, s2 <- w2 ]

coordinateToManhattanDistance :: Coordinate -> Int
coordinateToManhattanDistance (Coordinate x y) = (abs x) + (abs y)

main = do
    (filename:_) <- getArgs
    contents <- readFile filename
    let wireStrings = filter (\wire -> wire /= "") $ splitOn "\n" contents
    let wireDirections = map (\string -> splitOn "," string) wireStrings
    let (w1:w2:_) = map (coordinatesToSegments . directionsToCoordinates) wireDirections
    let intersections = getAllIntersections w1 w2
    let distance = map coordinateToManhattanDistance intersections
    let minDistance = minElement distance
    print minDistance