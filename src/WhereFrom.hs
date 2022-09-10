{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module WhereFrom where
import GHC.Stack.CCS as Stack
import qualified Data.Text as T
mkFrom :: a -> IO From
mkFrom a = do
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
parseLocation :: T.Text -> Maybe Location
parseLocation a 
  | T.null a = Nothing
  | otherwise = do
    let (file, rest) = T.breakOn ":" a
    let (start, end) = T.breakOn "-" $ T.drop 1 rest
    let (startLine, startCol) = T.breakOn "," start
    let (endLine, endCol) = T.breakOn "," $ T.drop 1 end
    pure $ Location file (read $ T.unpack startLine, read $ T.unpack startCol) (read $ T.unpack endLine, read $ T.unpack endCol)
type Loc = (Int, Int)
