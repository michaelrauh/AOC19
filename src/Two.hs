module Two where

import Control.Applicative
import Data.List
import Data.Maybe

main :: IO ()
main = do
  contents <- getContents
  let numbers = separateByComma contents
      allNumbers = getAllNumbers numbers 
      allNounsAndVerbs = liftA2 (,) [0..99] [0..99]
      readyLists = makeReadyLists allNumbers allNounsAndVerbs
      finalResults = makeFinalResults readyLists
      finalReturns = map fst finalResults
      indexOfAnswer = fromJust $ elemIndex desiredResult finalReturns
      answer = allNounsAndVerbs !! indexOfAnswer
      toPrint = 100 * fst answer + snd answer
  print toPrint 

makeReadyLists :: [Int] -> [(Int, Int)] -> [[Int]]
makeReadyLists allNumbers = map (setupInitialState allNumbers)

makeFinalResults :: [[Int]] -> [(Int, [Int])] 
makeFinalResults = map (\readyList -> followInstructions (0, readyList))

getAllNumbers :: [String] -> [Int]
getAllNumbers = map read

separateBy :: Eq a => a -> [a] -> [[a]]
separateBy chr = unfoldr sep where
  sep [] = Nothing
  sep l  = Just . fmap (drop 1) . break (== chr) $ l

separateByComma :: String -> [String]
separateByComma = separateBy ','

codeToOp :: Int -> (Int -> Int -> Int)
codeToOp x = if x == 1 then (+) else (*)

replace :: Int -> a -> [a] -> [a]
replace pos newVal list = take pos list ++ newVal : drop (pos+1) list

replaceWithInstruction :: (Int, [Int]) -> (Int, [Int])
replaceWithInstruction (pos, l) = 
  let op = codeToOp (l !! pos)
      firstOperand = (l !! (l !! (pos + 1)))
      secondOperand = (l !! (l !! (pos + 2)))
      writePosition = (l !! (pos + 3))
      toWrite = op firstOperand secondOperand
      resultList = replace writePosition toWrite l
  in (pos + 4, resultList) 

followInstructions :: (Int, [Int]) -> (Int, [Int])
followInstructions (pos, l) = if l !! pos == 99 then (head l, []) else followInstructions (replaceWithInstruction (pos, l))

setupInitialState l (noun, verb) = replace 2 verb (replace 1 noun l)

desiredResult = 19690720


