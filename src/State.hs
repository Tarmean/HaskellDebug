{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module State where


import GHC.Stack.CCS as Stack
import qualified GHC.HeapView as Heap
import qualified Data.Vector as V
import qualified Data.Text as T
import Control.Monad.Trans.Maybe
import AsyncRequests

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
mkFrom :: a -> IO From
mkFrom a = do
    [ipName, ipDesc, ipTyDesc, ipLabel, ipMod, loc] <- map T.pack <$> Stack.whereFrom a
    pure From {ipLoc = parseLocation loc, ..}
parseLocation :: T.Text -> Maybe Location
parseLocation a 
  | T.null a = Nothing
  | otherwise = do
    let (file, rest) = T.breakOn ":" a
    let (start, end) = T.breakOn "-" $ T.drop 1 rest
    let (startLine, startCol) = T.breakOn "," start
    let (endLine, endCol) = T.breakOn "," $ T.drop 1 end
    pure $ Location file (read $ T.unpack startLine, read $ T.unpack startCol) (read $ T.unpack endLine, read $ T.unpack endCol)


lookupCurrentNode :: CoreState t -> Maybe (Heap.HeapGraphEntry t)
lookupCurrentNode CoreState { _heapGraph = g, _activeNode = x@(_:_) } = Heap.lookupHeapGraph (last x) g
lookupCurrentNode _ = Nothing

type Loc = (Int, Int)
data CoreState a = CoreState {
  _heapGraph :: Heap.HeapGraph a,
  _activeNode :: [Heap.HeapGraphIndex]
} deriving (Show)
data RenderState = RenderState {
    _fileContent :: V.Vector T.Text,
    _fileImportant :: From
} deriving Show
data AppState a = AppState {
    _coreState :: CoreState a,
    _renderState :: RenderState
} deriving Show

data Requests a where
    LoadFile :: T.Text -> Requests (V.Vector T.Text)
    WhereFrom :: Heap.Box -> Requests From

loadRenderState :: MonadReq Requests m => CoreState a -> m (Maybe RenderState)
loadRenderState cs = runMaybeT $ do
    Just Heap.HeapGraphEntry{hgeBox = box} <- pure (lookupCurrentNode cs)
    Just from <- send (WhereFrom box)
    Just loc <- pure (ipLoc from)
    Just content <- send (LoadFile (lFile loc))
    pure $ RenderState content from


-- loadFrom :: Heap.HeapGraphIndex -> M t (Maybe From)
-- loadFrom idx =
--     use (aInfoMap . at idx) >>= \case
--         Just from -> pure (Just from)
--         Nothing ->
--             lookupEntry idx >>= \case
--                 Nothing -> pure Nothing
--                 Just (Heap.HeapGraphEntry { hgeBox = Heap.Box v}) -> do
--                     from <- liftIO $ mkFrom v
--                     aInfoMap %= M.insert idx from
--                     pure (Just from)
