{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Day8 where

import Text.ParserCombinators.ReadP
import Data.List.Extra
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup
import Data.Text (Text)
import GHC.TypeLits
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.IO as T

day8_1 :: IO ()
day8_1 = do
  pixels <- readInput
  let layers = toLayers pixels :: [Layer 25 6]
      layerWithLeast0s = minimumOn (countPixels Black) layers
      ones = countPixels White layerWithLeast0s
      twos = countPixels Transparent layerWithLeast0s
  print $ ones * twos

day8_2 :: IO ()
day8_2 = do
  pixels <- readInput
  let layers = toLayers pixels :: [Layer 25 6]
      picture = mergeLayers layers
  T.putStrLn $ renderLayer picture

data Pixel = Black
           | White
           | Transparent
           deriving (Eq, Show)

newtype Layer (h :: Nat) (w :: Nat) =
  Layer [Pixel]
  deriving Show

instance Semigroup Pixel where
  Transparent <> p = p
  p <> _ = p

instance Semigroup (Layer w h) where
  (Layer xs) <> (Layer ys) = Layer $ zipWith (<>) xs ys

renderPixel :: Pixel -> Text
renderPixel White = "⬜"
renderPixel Black = "⬛"
renderPixel Transparent =  " "

renderLayer :: forall w h.(KnownNat w, KnownNat h) => Layer w h -> Text
renderLayer (Layer ps) =
  let rows = chunksOf (fromIntegral $ natVal (Proxy :: Proxy w)) ps
  in T.unlines $ map (T.concat . map renderPixel) rows

mergeLayers :: [Layer w h] -> Layer w h
mergeLayers [] = error "No Layers found!"
mergeLayers (l:ls) = sconcat $ l :| ls

toLayers :: forall w h.(KnownNat w, KnownNat h) => [Pixel] -> [Layer w h]
toLayers pixels =
  let height = fromIntegral (natVal (Proxy :: Proxy h))
      width = fromIntegral (natVal (Proxy :: Proxy w))
  in Layer <$> chunksOf (height * width) pixels

countPixels :: Pixel -> Layer w h -> Int
countPixels pixel (Layer pixels) = length $ filter (== pixel) pixels

readInput :: IO [Pixel]
readInput = do
  inputStr <- getLine
  case readP_to_S inputReadP inputStr of
    [(pixels, "")] -> pure pixels
    _ -> error $ "Failed to parse '" ++ inputStr ++ "'"

inputReadP :: ReadP [Pixel]
inputReadP =
  (many1 $ choice [blackReadP, whiteReadP, transparentReadP]) <* eof

blackReadP :: ReadP Pixel
blackReadP = char '0' *> pure Black

whiteReadP :: ReadP Pixel
whiteReadP = char '1' *> pure White

transparentReadP :: ReadP Pixel
transparentReadP = char '2' *> pure Transparent
