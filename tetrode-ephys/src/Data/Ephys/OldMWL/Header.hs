{-# LANGUAGE OverloadedStrings #-}

module Data.Ephys.OldMWL.Header where

import qualified Data.Text            as T
import qualified Data.Map             as Map
import           Control.Applicative  hiding (many, (<|>))
import           Control.Monad
import           Text.Parsec
import           Text.Parsec.ByteString.Lazy
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as BSL
import           Data.Maybe (catMaybes)
import           Data.Char
import           System.IO 
import           Control.Lens hiding (noneOf)

-- Key-value store for MWL header data
type HeaderData = Map.Map BS.ByteString BS.ByteString

-- Should this be in Header.hs?  or somewhere else?
loadRawMWL :: FilePath -> IO (Either String (HeaderData, BSL.ByteString))
loadRawMWL fn = do
  r <- parseFromFile parseHeader fn
  case r of
    Left parseError     -> return $ Left (show parseError)
    Right headerAndRest -> return $ Right headerAndRest

fromRight (Right a) = a
  
parseHeader :: Parser (HeaderData, BSL.ByteString)
parseHeader = do
  h <- parseHeaderBlock
  r <- getInput
  let headerMap = foldr (\(k,v) -> Map.insert k v) Map.empty h
  return (headerMap,r)

parseHeaderBlock :: Parser  [(BS.ByteString,BS.ByteString)]
parseHeaderBlock = parseIntro *> 
                   (catMaybes <$> many (try parseLine)) <*
                   parseOutro
              
parseIntro :: Parser String
parseIntro = string "%%BEGINHEADER\n"

parseOutro :: Parser String
parseOutro = string "%%ENDHEADER\n"

parseLine :: Parser (Maybe (BS.ByteString,BS.ByteString))
parseLine = char '%' *> 
            (try (char ' ' *> parsePair)
             <|> parseBadXYDATALine
             <|> (char ' ' *> parseInfoLine) 
             <|> try parseBlank ) 
            <* char '\n'

parseBlank :: Parser (Maybe (BS.ByteString,BS.ByteString))
parseBlank = pure Nothing

parseInfoLine :: Parser (Maybe (BS.ByteString,BS.ByteString))
parseInfoLine = many1 (noneOf "\n") *> pure Nothing

parseBadXYDATALine :: Parser (Maybe (BS.ByteString,BS.ByteString))
parseBadXYDATALine = string "%XYDATAFILE" *>
                     many (noneOf "\n") *>
                     pure Nothing

parsePair :: Parser (Maybe (BS.ByteString,BS.ByteString))
parsePair = Just <$> ((,) <$> keyToken <*> valueToken)
  where keyToken   = BSC.map toLower . BSC.pack 
                     <$> (many1 (noneOf ":\n")  <* char ':')
        valueToken = BSC.pack <$> (many (space <|> tab) 
                                  *> (many1 (noneOf "\n"))) 
        
