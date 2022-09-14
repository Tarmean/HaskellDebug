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
    ls <- map T.pack <$> Stack.whereFrom a
    case ls of
        [ipName, ipDesc, ipTyDesc, ipLabel, ipMod, loc] -> pure From {ipLoc = parseLocation loc, ..}
        [] -> pure $ From "?" "?" "?" "?" "" Nothing
        o -> error ("mkFrom: " ++ show o)

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
    (l,r) <- try pLoc1 <|> try pLoc2 <|> pLoc3
    pure $ Location file l r
  where
    pLoc1 = do
        _ <- char '('
        startLine <- pNum
        _ <- char ','
        startCol <- pNum
        _ <- char ')'
        _ <- char '-'
        _ <- char '('
        endLine <- pNum
        _ <- char ','
        endCol <- pNum
        _ <- char ')'
        pure ((startLine, startCol), (endLine, endCol))
    pLoc2 = do
        startLine <- pNum
        _ <- char ':'
        startCol <- pNum
        _ <- char '-'
        endCol <- pNum
        pure ((startLine, startCol), (startLine, endCol))
    pLoc3 = do
        startLine <- pNum
        _ <- char ':'
        startCol <- pNum
        pure ((startLine, startCol), (startLine, startCol))

parseLocation :: T.Text -> Maybe Location
parseLocation a 
  | T.null a = Nothing
  | otherwise = case runParser pLocation a of
      Left e -> error e
      Right o -> Just o
type Loc = (Int, Int)
