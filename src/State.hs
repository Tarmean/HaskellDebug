{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module State where


import qualified GHC.HeapView as Heap
import qualified Data.Vector as V
import qualified Data.Text as T
import AsyncRequests ( MonadReq(..), Caching, AsyncRequests, Cache, runCachingT )
import WhereFrom ( From(ipLoc), Location(lFile) )
import Lens.Micro ( Lens', lens )
import qualified Data.Text.IO as T
import Debug.Trace (traceM)
import Control.Monad.Trans (MonadIO)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE


lookupCurrentNode :: CoreState -> Maybe (Heap.HeapGraphEntry HeapData)
lookupCurrentNode CoreState { _heapGraph = g, _activeNode = x } = Heap.lookupHeapGraph (NE.head x) g

type HeapData = (Maybe From, [String])
data CoreState = CoreState {
  _heapGraph :: Heap.HeapGraph HeapData,
  _activeNode :: NonEmpty Heap.HeapGraphIndex
} deriving (Show)
data RenderState = RenderState {
    _fileContent :: Maybe (V.Vector T.Text),
    _fileImportant :: Maybe From
} deriving Show
data Requests a where
    LoadFile :: T.Text -> Requests (V.Vector T.Text)
deriving instance Show (Requests a)
data AppState = AppState {
    _coreState :: CoreState,
    _renderState :: RenderState,
    _runAsync :: AsyncRequests Requests
}
heapGraph :: Lens' CoreState (Heap.HeapGraph HeapData)
heapGraph = lens _heapGraph (\s v -> s { _heapGraph = v })
activeNode :: Lens' CoreState (NonEmpty Heap.HeapGraphIndex)
activeNode = lens _activeNode (\s v -> s { _activeNode = v })
fileContent :: Lens' RenderState (Maybe (V.Vector T.Text))
fileContent = lens _fileContent (\s v -> s { _fileContent = v })
fileImportant :: Lens' RenderState (Maybe From)
fileImportant = lens _fileImportant (\s v -> s { _fileImportant = v })
coreState :: Lens' AppState CoreState
coreState = lens _coreState (\s v -> s { _coreState = v })
renderState :: Lens' AppState RenderState
renderState = lens _renderState (\s v -> s { _renderState = v })
runAsync :: Lens' AppState (AsyncRequests Requests)
runAsync = lens _runAsync (\s v -> s { _runAsync = v })


runRequest :: Requests x -> IO x
runRequest (LoadFile path) = do
    traceM $ "Loading file: " <> T.unpack path
    V.fromList  . T.lines . T.filter (/= '\r') <$> T.readFile (T.unpack path)

deriving instance Eq (Requests a)
instance Ord (Requests a) where
    compare (LoadFile a) (LoadFile b) = compare a b

loadRenderState :: MonadIO m => CoreState -> Cache Requests -> m RenderState
loadRenderState cs c = do
    let from = fst . Heap.hgeData =<< lookupCurrentNode cs 
    content <- runCachingT c $ with (fmap lFile . ipLoc =<< from) (\loc -> send (LoadFile loc))
    pure $ RenderState content from

rebuildState :: MonadIO m => AppState -> m AppState
rebuildState as = do
    rs <- loadRenderState (_coreState as) (_runAsync as)
    pure $ AppState (_coreState as) rs (_runAsync as)

with :: Applicative m => Maybe a -> (a -> m (Maybe c)) -> m (Maybe c)
with Nothing _ = pure Nothing
with (Just a) g = g a

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
