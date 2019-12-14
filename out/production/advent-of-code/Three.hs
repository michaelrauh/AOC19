module Three where

import Data.List

data WireDirection = U Integer | L Integer | D Integer | R Integer deriving Show
data WireLocation = Location Integer Integer deriving (Eq, Show)
type VisitedLocations = [WireLocation]

instance Ord WireLocation where
  compare (Location a b) (Location c d) = compare (abs a + abs b) (abs c + abs d)

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
navigate (Location x y) (U m) = tail $ fromTupleList $ zip (repeat x) [y, (y + 1)..(y + m)]
navigate (Location x y) (D m) = tail $ fromTupleList $ zip (repeat x) [y, (y - 1)..(y - m)]
navigate (Location x y) (L m) = tail $ fromTupleList $ zip [x, (x - 1)..(x - m)] (repeat y)
navigate (Location x y) (R m) = tail $ fromTupleList $ zip [x, (x + 1)..(x + m)] (repeat y)

fromTupleList :: [(Integer, Integer)] -> [WireLocation]
fromTupleList = map $ uncurry Location

go :: [WireDirection] -> VisitedLocations
go = foldl' foldingFunction [Location 0 0]

foldingFunction :: VisitedLocations -> WireDirection -> VisitedLocations
foldingFunction alreadyVisited wireDirection =
  let newLocations = navigate (last alreadyVisited) wireDirection
   in alreadyVisited ++ newLocations

drawDiagrams :: [[WireDirection]] -> [VisitedLocations]
drawDiagrams = map go

findOverlaps :: [VisitedLocations] -> [WireLocation]
findOverlaps [x, xs] = tail $ x `intersect` xs

findNearestToOrigin :: [WireLocation] -> WireLocation
findNearestToOrigin = minimum

findAnswer :: String -> Integer
findAnswer = toManhattanDistance . findNearestToOrigin . findOverlaps . drawDiagrams . parse

allAnswers = findOverlaps . drawDiagrams . parse

toManhattanDistance :: WireLocation -> Integer
toManhattanDistance (Location x y) = abs x + abs y