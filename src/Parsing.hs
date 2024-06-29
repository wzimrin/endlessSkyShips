module Parsing ( file
               , rawFile
               , parseFile
               , ESBlock(..)
               , ESBlocks
               ) where

import Control.Monad
import Data.Functor
import qualified Data.Map as Map
import Text.Parsec
import Text.Parsec.String

data RawBlock = RawBlock String [String] [RawBlock]
  deriving Show

data ESBlock = ESBlock [String] ESBlocks
  deriving Show

type ESBlocks = Map.Map String [ESBlock]

comment :: Parser [t]
comment = do
  _ <- string "#"
  _ <- manyTill anyChar (try endOfLine)
  return []

single :: Parser String
single = many1 (alphaNum <|> oneOf "-.")

quoted :: Parser String
quoted = between (string "\"") (string "\"") $ many1 $ noneOf "\""

backquoted :: Parser String
backquoted = between (string "`") (string "`") $ many1 $ noneOf "`"

item :: Parser String
item = single <|> quoted <|> backquoted

nextLine :: Parser ()
nextLine = skipMany1 $ try (skipMany (oneOf " \t") *> endOfLine)

indent :: Int -> Parser ()
indent n = void $ count n (oneOf " \t")

gap :: Parser ()
gap = skipMany1 (string " ")

line :: Int -> Parser [String]
line n = do
  indent n
  result <- sepBy1 item gap
  nextLine
  return result

rawBlock :: Int -> Parser RawBlock
rawBlock n = do
  (blockName : vals) <- line n
  children <- many (try (rawBlock (n+1)))
  return $ RawBlock blockName vals children

convertBlocks :: [RawBlock] -> ESBlocks
convertBlocks = Map.fromListWith (++) . map blockToPair
  where
    blockToPair (RawBlock name' vals children) = (name', [ESBlock vals (convertBlocks children)])

rawFile :: Parser [RawBlock]
rawFile = join <$> many (comment <|> (nextLine $> []) <|> fmap pure (rawBlock 0))

file :: Parser ESBlocks
file = convertBlocks <$> rawFile

parseFile :: String -> IO ESBlocks
parseFile path = either throw id <$> parseFromFile file path
  where throw err = error ("Parse error " ++ path ++ " " ++ show err)
