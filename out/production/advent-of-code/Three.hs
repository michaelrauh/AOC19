module Three where

import Data.List

data WireDirection = U Integer | L Integer | D Integer | R Integer deriving Show
data WireLocation = Location Integer Integer deriving (Eq, Show)
data VisitedLocations = Visited WireLocation [WireLocation]

instance Ord WireLocation where
  compare (Location a b) (Location c d) = compare (a + b) (c + d)

separateBy :: Eq a => a -> [a] -> [[a]]
separateBy chr = unfoldr sep where
  sep [] = Nothing
  sep l  = Just . fmap (drop 1) . break (== chr) $ l

separateByNewline = separateBy '\n'

separateByComma = separateBy ','

toWireDirections :: String -> WireDirection
toWireDirections ('U':xs) = U $ read xs
toWireDirections ('D':xs) = D $ read xs
toWireDirections ('L':xs) = L $ read xs
toWireDirections ('R':xs) = R $ read xs

parse :: String -> [[WireDirection]]
parse x = map (map toWireDirections . separateByComma) $ separateByNewline x

navigate :: WireLocation -> WireDirection -> [WireLocation]
navigate (Location x y) (U m) = fromTupleList $ zip (repeat x) [y..(y + m)]
navigate (Location x y) (D m) = fromTupleList $ zip (repeat x) [y,y + signum ((-m) - y) .. (-m)]
navigate (Location x y) (L m) = fromTupleList $ zip [x,x + signum ((-m) - x) .. (-m)] (repeat y)
navigate (Location x y) (R m) = fromTupleList $ zip [x..(x + m)] (repeat y)

fromTupleList :: [(Integer, Integer)] -> [WireLocation]
fromTupleList = map (uncurry Location)

go :: [WireDirection] -> VisitedLocations
go = foldr foldingFunction (Visited  (Location 0 0) [])

foldingFunction :: WireDirection -> VisitedLocations -> VisitedLocations
foldingFunction wireDirection (Visited currentLocation alreadyVisited) =
  let newLocations = navigate currentLocation wireDirection
  in Visited (last newLocations) (alreadyVisited ++ newLocations)

drawDiagrams :: [[WireDirection]] -> [VisitedLocations]
drawDiagrams = map go

findOverlaps :: [VisitedLocations] -> [WireLocation]
findOverlaps visitedLocationsList = 
  let allWireLocations = map getAllVisitedLocations visitedLocationsList
      allUniques = map nub allWireLocations
      flatUniques = concat allUniques
      grouped = group flatUniques
      answers = filter (\x -> length x > 1) grouped
      finals = map head answers
  in finals

findNearestToOrigin :: [WireLocation] -> WireLocation
findNearestToOrigin = minimum

findAnswer :: String -> Integer
findAnswer = toManhattanDistance . findNearestToOrigin . findOverlaps . drawDiagrams . parse

toManhattanDistance :: WireLocation -> Integer
toManhattanDistance (Location x y) = x + y 

getAllVisitedLocations :: VisitedLocations -> [WireLocation]
getAllVisitedLocations (Visited l a) = a