module One where

main :: IO ()
main = do
  contents <- getContents
  let numbers = words contents
      allNumbers = map (calculateFuel . read) numbers
  print $ sum allNumbers
  return ()

calculateFuel :: Integer -> Integer
calculateFuel mass =
  let requiredFuel = (mass `div` 3) - 2
  in if positive requiredFuel then requiredFuel + calculateFuel requiredFuel else 0

positive :: (Num a, Ord a) => a -> Bool
positive x = x > 0

