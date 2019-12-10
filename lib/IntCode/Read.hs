module IntCode.Read where

import Data.IntMap
import Data.Char
import IntCode.Types
import Text.ParserCombinators.ReadP

parseMemory :: IO Code
parseMemory = do
  inputStr <- getLine
  case readP_to_S memoryReadP inputStr of
    [(ints, "")] -> pure $ fromList $ zip [0..(length ints -1)] ints
    _ -> error $ "Failed to parse '" ++ inputStr ++ "'"

memoryReadP :: ReadP [Int]
memoryReadP = do
  sepBy1 intReadP commaReadP <* eof

intReadP :: ReadP Int
intReadP = do
  signed <- choice [Just <$> char '-', pure Nothing]
  intStr <- many1 $ satisfy isDigit
  case signed of
    Nothing -> pure $ read intStr
    Just _ -> pure $ read ('-':intStr)

commaReadP :: ReadP ()
commaReadP = satisfy (== ',') *> pure ()
