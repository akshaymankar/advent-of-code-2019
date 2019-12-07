module Day6 where

import Data.Map                     (Map, keys, findWithDefault, fromListWith)
import Data.Maybe                   (fromJust, catMaybes)
import Text.ParserCombinators.ReadP

import qualified Data.Map as M

type Orbiter = String
type Orbitee = String

day6_1 :: IO ()
day6_1 = do
  input <- readInput
  print $ countOrbiters input "COM"

day6_2 :: IO ()
day6_2 = do
  input <- readInput
  let source = findOrbitee "YOU" input
  let destination = findOrbitee "SAN" input
  print $ findShortestPath input source destination

countOrbiters :: Map Orbitee [Orbiter] -> Orbitee -> Int
countOrbiters =
  go 1
  where
    go n m o =
      let orbiters = findOrbiters o m
      in (n * length orbiters) + (sum $ map (go (n + 1) m) orbiters)

findOrbitee :: Orbiter -> Map Orbitee [Orbiter] -> Orbitee
findOrbitee orbiter orbits =
  let potentialOrbitees = keys $ M.filter (elem orbiter) orbits
  in case potentialOrbitees of
       [orbitee] -> orbitee
       xs -> "expected exactly one potential orbitee, but found " ++ show xs

findOrbiters :: Orbitee -> Map Orbitee [Orbiter] -> [Orbiter]
findOrbiters = findWithDefault []

findShortestPath :: Map Orbitee [Orbiter] -> Orbitee -> Orbitee -> Int
findShortestPath m yourOrbitee santasOrbitee =
  fromJust $ go "YOU" santasOrbitee yourOrbitee
  where
    go from dest src
      | src == dest = Just 0
      | otherwise =
        let neighbours = (findOrbitee src m):(findOrbiters src m)
        in case filter (/= from) neighbours of
             [] -> Nothing
             xs ->
               case catMaybes $ map (go src dest) xs of
                 [] -> Nothing
                 [found] -> Just (1 + found)
                 _ -> error "Expected there to be exactly one path!"

readInput :: IO (Map Orbitee [Orbiter])
readInput = do
  inputStr <- getContents
  let orbits = map readOrbit $ lines inputStr
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
