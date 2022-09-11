{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module WhereFrom where
import GHC.Stack.CCS as Stack
import qualified Data.Text as T
import GHC.HeapView (Box(..))
import Text.Megaparsec (Parsec, some, satisfy, parse, errorBundlePretty, MonadParsec (try), (<|>))
import Text.Megaparsec.Char (char)
import Data.Char (isDigit)
import qualified Text.Megaparsec as P
import Data.Void
mkFrom :: Box -> IO From
mkFrom (Box a) = do
    [ipName, ipDesc, ipTyDesc, ipLabel, ipMod, loc] <- map T.pack <$> Stack.whereFrom a
    pure From {ipLoc = parseLocation loc, ..}

data Location = Location {
   lFile :: T.Text,
   lStart :: Loc,
   lEnd :: Loc
} deriving (Show, Eq, Ord)
data From = From {
    ipName :: T.Text,
    ipDesc :: T.Text,
    ipTyDesc :: T.Text,
    ipLabel :: T.Text,
    ipMod :: T.Text,
    ipLoc :: Maybe Location
} deriving (Show, Eq, Ord)

type Parser = Parsec Void T.Text
runParser :: Parser a -> T.Text -> Either String a
runParser p t = case parse p "" t of
    Left e -> Left $ P.errorBundlePretty e
    Right a -> Right a
pNum :: Parser Int
pNum = read . T.unpack <$> P.takeWhile1P (Just "Number") isDigit
pLocation :: Parser Location
pLocation = do
    file <- P.takeWhile1P (Just "FilePath") (/= ':')
    _ <- char ':'
    (l,r) <- try pLoc1 <|> pLoc2
    pure $ Location file l r
  where
    pLoc1 = do
        startLine <- pNum
        _ <- char ':'
        startCol <- pNum
        _ <- char '-'
        endLine <- pNum
        _ <- char ':'
        endCol <- pNum
        pure ((startLine, startCol), (endLine, endCol))
    pLoc2 = do
        startLine <- pNum
        _ <- char ':'
        startCol <- pNum
        _ <- char '-'
        endCol <- pNum
        pure ((startLine, startCol), (startLine, endCol))

parseLocation :: T.Text -> Maybe Location
parseLocation a 
  | T.null a = Nothing
  | otherwise = case runParser pLocation a of
      Left e -> error e
      Right o -> Just o
type Loc = (Int, Int)
