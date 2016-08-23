-- The three functions defined here ( firstTrip - thirdTrip)
-- each take out they're corresponding part of a triple i.e.
-- secondTrip (1,2,3) would return 2

firstTrip :: (a,b,c) -> a
firstTrip (a,_,_) = a

secondTrip :: (a,b,c) -> b
secondTrip (_,b,_) = b

thirdTrip :: (a,b,c) -> c
thirdTrip (_,_,c) = c

findVectors :: [(Float, Float, Float)] -> [(Float, Float, Float)]
-- findVectors inputs a list of three points as triples
-- i.e. [ Point A, Point B, Point C] and outputs a list 
-- of two vectors [AB, AC]
findVectors coordList = 
    if length coordList == 3 
    then do
        let firstPoint = head coordList
        let secPoint= head (tail coordList)
        let finPoint = last coordList
        let vectOne = (firstTrip secPoint - firstTrip firstPoint, secondTrip secPoint - secondTrip firstPoint, thirdTrip secPoint - thirdTrip firstPoint)
        let vectTwo = (firstTrip finPoint - firstTrip firstPoint, secondTrip finPoint - secondTrip firstPoint, thirdTrip finPoint - thirdTrip firstPoint)
        [vectOne,vectTwo]
    else
        coordList

crossVects :: [(Float,Float,Float)] -> (Float,Float,Float)
-- crossVects takes in a list of float triples and returns 
-- a single float triple that represents the vector of the
-- cross product between the vectors input 
-- if the length of the list is longer than two then only the
-- first two elements will be used 
crossVects vectList = 
    do
    let firstVect = head(vectList)
    let secVect = head (tail vectList)
    let xValue = (secondTrip firstVect * thirdTrip secVect) - (thirdTrip firstVect * secondTrip secVect)
    let yValue = (thirdTrip firstVect * firstTrip secVect) - (firstTrip firstVect * thirdTrip secVect)
    let zValue = (firstTrip firstVect * secondTrip secVect) - (secondTrip firstVect * firstTrip secVect)
    (xValue,yValue,zValue)

offSet :: (Float,Float,Float) -> [(Float,Float,Float)]-> Float
-- Calculates the offset to find exact plane, not just
-- a parallel plane
offSet crossVector pointList = 
    do
    let point = head pointList
    (firstTrip point * firstTrip crossVector + 
        secondTrip point * secondTrip crossVector + 
        thirdTrip point * thirdTrip crossVector) * (0-1)

toString :: (Float,Float,Float) -> [String]
toString trip = 
    do
    let strings = [show (firstTrip trip), show (secondTrip trip), show (thirdTrip trip)]
    strings

planeEq :: [String] -> Float -> String
--returns the plane equation for the given points
planeEq crossVector difference = head crossVector ++ "x + "
    ++ head (tail crossVector) ++ "y + "
    ++ last (crossVector) ++ "z + "
    ++ show difference ++ " = 0"

findPlane :: [(Float,Float,Float)] -> String
-- inputs a list of three triples, where the values of the triples are all Floats
-- ouputs the plane equation as a string
findPlane floatList = do
    let cross = crossVects (findVectors floatList)
    let off = offSet cross floatList
    planeEq (toString cross) off
