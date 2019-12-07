module Day6 where

import Text.ParserCombinators.ReadP
import Data.Map

type Orbiter = String
type Orbitee = String

day6_1 :: IO ()
day6_1 = do
  input <- readInput
  print $ countOrbiters input "COM"

countOrbiters :: Map Orbitee [Orbiter] -> Orbitee -> Int
countOrbiters =
  go 1
  where
    go n m o =
      let orbiters = findWithDefault [] o m
      in (n * length orbiters) + (sum $ Prelude.map (go (n + 1) m) orbiters)

readInput :: IO (Map Orbitee [Orbiter])
readInput = do
  inputStr <- getContents
  let orbits = Prelude.map readOrbit $ lines inputStr
  pure $ fromListWith (++) orbits

readOrbit :: String -> (Orbitee, [Orbiter])
readOrbit str = do
  case readP_to_S (orbitReadP <* eof) str of
    [(orbit, "")] -> orbit
    x -> error $ "Failed to parse '" ++ str ++ "'" ++ show x

orbitReadP :: ReadP (Orbitee, [Orbiter])
orbitReadP = do
  orbitee <- munch1 (/= ')')
  _ <- char ')'
  orbiter <- munch1 (const True)
  return $ (orbitee,  [orbiter])
