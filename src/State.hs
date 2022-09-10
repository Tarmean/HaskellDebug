{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module State where


import qualified GHC.HeapView as Heap
import qualified Data.Vector as V
import qualified Data.Text as T
import Control.Monad.Trans.Maybe
import AsyncRequests
import WhereFrom



lookupCurrentNode :: CoreState -> Maybe (Heap.HeapGraphEntry From)
lookupCurrentNode CoreState { _heapGraph = g, _activeNode = x@(_:_) } = Heap.lookupHeapGraph (last x) g
lookupCurrentNode _ = Nothing

data CoreState = CoreState {
  _heapGraph :: Heap.HeapGraph From,
  _activeNode :: [Heap.HeapGraphIndex]
} deriving (Show)
data RenderState = RenderState {
    _fileContent :: V.Vector T.Text,
    _fileImportant :: From
} deriving Show
data AppState = AppState {
    _coreState :: CoreState,
    _renderState :: RenderState
} deriving Show

data Requests a where
    LoadFile :: T.Text -> Requests (V.Vector T.Text)

loadRenderState :: MonadReq Requests m => CoreState -> m (Maybe RenderState)
loadRenderState cs = runMaybeT $ do
    Just Heap.HeapGraphEntry{Heap.hgeData = from} <- pure (lookupCurrentNode cs)
    Just loc <- pure (ipLoc from)
    Just content <- send (LoadFile (lFile loc))
    pure $ RenderState content from


-- loadFrom :: Heap.HeapGraphIndex -> M (Maybe From)
-- loadFrom idx =
--     use (aInfoMap . at idx) >>= \case
--         Just from -> pure (Just from)
--         Nothing ->
--             lookupEntry idx >>= \case
--                 Nothing -> pure Nothing
--                 Just (Heap.HeapGraphEntry { Heap.hgeData = dat}) -> pure (Just dat)
