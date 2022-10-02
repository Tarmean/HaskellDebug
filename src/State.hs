{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- {-# OPTIONS_GHC -fplugin GhcDump.Plugin #-}
module State where


import qualified GHC.HeapView as Heap
import qualified Data.Vector as V
import qualified Data.Text as T
import AsyncRequests ( MonadReq(..), AsyncRequests, Cache, runCachingT )
import WhereFrom ( From(..), Location(lFile) )
import Lens.Micro ( Lens', lens )
import qualified Data.Text.IO as T
import Debug.Trace (traceM)
import Control.Monad.Trans (MonadIO)
import Control.Monad.Except
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified GhcDump.Util as R
import GhcDump.Ast (Module, Binder (unBndr), Module' (moduleTopBindings), TopBinding, Binder' (binderName), TopBinding' (..), SModule, SBinder (unSBndr), readSModule)
import qualified Data.List as L
import Control.Monad.Trans.Maybe
import Control.Applicative (empty)
import qualified Prettyprinter.Render.String as Str
import GhcDump.Pretty ( Pretty(pretty) )
import Data.Text.Prettyprint.Doc (defaultLayoutOptions, layoutPretty)
import qualified GhcDump.Ast as R
import System.Directory
import Debug.Pretty.Simple (pTraceShowM)
import Control.Monad (join)
import GhcDump.Reconstruct (reconModule)


lookupCurrentNode :: CoreState -> Maybe (Heap.HeapGraphEntry HeapData)
lookupCurrentNode CoreState { _heapGraph = g, _activeNode = x } = Heap.lookupHeapGraph (NE.head x) g

data ViewPorts = ViewPortFile | ViewPortHeap | ViewPortCore
  deriving (Eq, Ord, Show)
data HeapData = HeapData { hsourceLoc :: Maybe From, hwhoCreated ::  [String] }
  deriving  (Eq, Ord, Show)
data CoreState = CoreState {
  _heapGraph :: Heap.HeapGraph HeapData,
  _activeNode :: NonEmpty Heap.HeapGraphIndex,
  _coreKind :: Int,
  _focusView :: Maybe ViewPorts
} deriving (Show)
data RenderState = RenderState {
    _fileContent :: Maybe (V.Vector T.Text),
    _astContent :: Either String T.Text,
    _fileImportant :: Maybe From
} deriving (Eq, Ord, Show)
data Requests a where
    LoadFile :: T.Text -> Requests (Maybe (V.Vector T.Text))
    LoadGhcDump :: T.Text -> Int -> Requests (Either String T.Text)
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
focusView :: Lens' CoreState (Maybe ViewPorts)
focusView = lens _focusView (\s v -> s { _focusView = v })
fileContent :: Lens' RenderState (Maybe (V.Vector T.Text))
coreKind :: Lens' CoreState Int
coreKind = lens _coreKind (\s v -> s { _coreKind = v })
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
    doesFileExist (T.unpack path) >>= \case
      False -> pure Nothing
      True -> Just . V.fromList  . T.lines . T.filter (/= '\r') <$> T.readFile (T.unpack path)
runRequest (LoadGhcDump modul v) = do
    let
       path0 = dropEnd ".hs" modul
       pad4 s = replicate (4 - length s) '0' <> s
    let prefix = "dist-newstyle/src/"
    let path = prefix <> T.unpack path0 <> ".dump-prep" -- ".pass-" <> pad4 (show v) <> ".cbor.zstd"
    doesFileExist path >>= \case
      True -> do
        Right <$> T.readFile path
        -- error (renderShowS (layoutPretty defaultLayoutOptions (pretty o)) "")
        -- pure (Just $ reconModule o)
      False -> pure (Left $ "File not found: " <> path)

deriving instance Eq (Requests a)
deriving instance Ord (Requests a)

dropEnd :: T.Text -> T.Text -> T.Text
dropEnd end str
    | T.isSuffixOf end str = T.dropEnd (T.length end) str
    |otherwise = str
loadRenderState :: MonadIO m => CoreState -> Cache Requests -> m RenderState
loadRenderState cs c = do
    let from = hsourceLoc . Heap.hgeData =<< lookupCurrentNode cs 
    content <- runCachingT c $ with (fmap lFile . ipLoc =<< from) (\loc -> send (LoadFile loc))
    ast <- do
          case from of
            Nothing -> pure (Left "No source location")
            Just WhereFrom.From {..} -> do
              runCachingT c $ send (LoadGhcDump ipMod (_coreKind cs)) >>= \case
                  Just a -> pure a
                  Nothing -> pure (Left "No dump")
        --   let 
        --     binderTxt = binderName  . unBndr 
        --     hasBinder s (NonRecTopBinding b _ _) = T.isInfixOf s (binderTxt b)
        --     hasBinder s (RecTopBinding ls) = or [T.isInfixOf s (binderTxt x) | (x,_,_) <- ls]
        --   case L.find (hasBinder (dropEnd "_info" $ ipName frm)) (moduleTopBindings o) of
        --       Nothing -> --if
        --         empty
        --         --  T.isInfixOf "con" (ipName frm) || (ipName frm == "sat_info")
        --         --  then empty
        --         --  else error ("No binder found for " <> show frm <> "\n" <> show o)
        --       Just x -> pure x
    pure $ RenderState (join content) ast from

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
