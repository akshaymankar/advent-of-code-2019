module Day14 where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Char
import Text.ParserCombinators.ReadP

day14_1 :: IO ()
day14_1 = do
  chart <- readInput
  print $ oreRequired chart 1

day14_2 :: IO ()
day14_2 = do
  chart <- readInput
  let orePerFuel = oreRequired chart 1
      low = 1000000000000 `div` orePerFuel
      high = low * 2
  print $ boundedSearch chart low high

type Chem = String
type ReactionLHS = [(Chem, Int)]
type ProducerChart = Map Chem (ReactionLHS, Int)
type Repository = Map Chem Int

boundedSearch :: ProducerChart -> Int -> Int -> Int
boundedSearch chart =
  go
  where
    go :: Int -> Int -> Int
    go low high
      | low == high = low
      | high - low == 1 =
          if (oreRequired chart high) > 1000000000000
          then low
          else high
      | otherwise =
          let mid = (low + ((high - low ) `div` 2))
          in if (oreRequired chart mid) > 1000000000000
             then go low mid
             else go mid high

oreRequired :: ProducerChart -> Int -> Int
oreRequired chart fuelRequired =
  go M.empty [("FUEL", fuelRequired)]
  where
    go :: Repository -> [(Chem, Int)] -> Int
    go _ [] = undefined
    go _ [("ORE", qty)] = qty
    go repo (("ORE", qty):rest) = qty + go repo rest
    go repo ((chem,qty):rest) =
      let (moreChems, produced) = chart M.! chem
          reactionsRequired :: Int
          reactionsRequired = ceiling $ (fromIntegral qty :: Double) / (fromIntegral produced)
          totalChemsRequired = multiplyChems reactionsRequired moreChems
          (repoAfterTaking, chemsRequired) = takeFromRepo repo totalChemsRequired
          totalProduced = reactionsRequired * produced
          excess = totalProduced - qty
          repoAfterDeposit = M.insertWith (+) chem excess repoAfterTaking
      in go repoAfterDeposit $ chemsRequired ++ rest

takeFromRepo :: Repository -> [(Chem, Int)] -> (Repository, [(Chem,Int)])
takeFromRepo repo [] = (repo, [])
takeFromRepo repo ((chem,qty):rest) =
  case M.lookup chem repo of
    Nothing ->
      let (newRepo, chems) = takeFromRepo repo rest
      in (newRepo, (chem,qty):chems)
    Just qtyInRepo ->
      if qtyInRepo >= qty
      then let newRepo = M.insert chem (qtyInRepo - qty) repo
           in takeFromRepo newRepo rest
      else let (newRepo, chems) = takeFromRepo (M.delete chem repo) rest
               remainingChem = (chem, qty - qtyInRepo)
           in (newRepo, remainingChem:chems)

multiplyChems :: Int -> ReactionLHS -> ReactionLHS
multiplyChems _ [] = []
multiplyChems m ((chem, qty):rest) = (chem, qty * m):multiplyChems m rest

readInput :: IO ProducerChart
readInput = do
  inputStr <- getContents
  let reactions = map readReaction $ lines inputStr
  return $ M.fromList reactions

readReaction :: String -> (Chem, (ReactionLHS, Int))
readReaction str = do
  case readP_to_S (reactionReadP <* eof) str of
    [(reaction, "")] -> reaction
    x -> error $ "Failed to parse '" ++ str ++ "'" ++ show x

reactionReadP :: ReadP (Chem, (ReactionLHS, Int))
reactionReadP = do
  lhs <- reactionLHSReadP
  _ <- char ' ' >> char '=' >> char '>' >> char ' '
  (chem, qty) <- reactionRHSReadP
  return (chem, (lhs, qty))

reactionLHSReadP :: ReadP ReactionLHS
reactionLHSReadP = sepBy1 quantityChemPair (char ',' >> char ' ')

reactionRHSReadP :: ReadP (Chem, Int)
reactionRHSReadP = quantityChemPair

intReadP :: ReadP Int
intReadP = do
  signed <- choice [Just <$> char '-', pure Nothing]
  intStr <- many1 $ satisfy isDigit
  case signed of
    Nothing -> pure $ read intStr
    Just _ -> pure $ read ('-':intStr)

chemReadP :: ReadP Chem
chemReadP = do
  munch1 isAlpha

quantityChemPair :: ReadP (Chem, Int)
quantityChemPair = do
  quantity <- intReadP
  _ <- char ' '
  chem <- chemReadP
  pure (chem, quantity)
