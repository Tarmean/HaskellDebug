{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module State where


import GHC.Stack.CCS as Stack
import qualified GHC.HeapView as Heap
import qualified Data.Vector as V
import qualified Data.Text as T
import Control.Monad.Trans.Maybe
import AsyncRequests
import WhereFrom
import Lens.Micro.TH



lookupCurrentNode :: CoreState -> Maybe (Heap.HeapGraphEntry From)
lookupCurrentNode CoreState { _heapGraph = g, _activeNode = x@(_:_) } = Heap.lookupHeapGraph (last x) g
lookupCurrentNode _ = Nothing

data CoreState = CoreState {
  _heapGraph :: Heap.HeapGraph From,
  _activeNode :: [Heap.HeapGraphIndex]
} deriving (Show)
data RenderState = RenderState {
    _fileContent :: Maybe (V.Vector T.Text),
    _fileImportant :: Maybe From
} deriving Show
data AppState = AppState {
    _coreState :: CoreState,
    _renderState :: RenderState
} deriving Show
makeLenses ''CoreState
makeLenses ''RenderState
makeLenses ''AppState

data Requests a where
    LoadFile :: T.Text -> Requests (V.Vector T.Text)
deriving instance Ord (Requests a) where
    compare (LoadFile a) (LoadFile b) = compare a b
    compare (LoadFile _) _ = LT
    compare _ (LoadFile _) = GT

loadRenderState :: MonadReq Requests m => CoreState -> m (Maybe RenderState)
loadRenderState cs = do
    Heap.HeapGraphEntry{Heap.hgeData = from} <- pure (lookupCurrentNode cs)
    content <- with' ipLoc from (\loc -> send (LoadFile (lFile loc)))
    pure $ RenderState content from


with :: Applicative m => (a -> b) -> Maybe a -> (b -> m (Maybe c)) -> m (Maybe c)
with f Nothing g = pure Nothing
with f (Just a) g = g (f a)
with' :: Applicative m => (a -> Maybe b) -> Maybe a -> (b -> m (Maybe c)) -> m (Maybe c)
with' f Nothing g = pure Nothing
with' f (Just a) g = case (f a) of
    Nothing -> pure Nothing
    Just b -> g b

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
