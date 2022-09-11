{-# LANGUAGE GADTs #-}
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
import Lens.Micro.TH

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
    _fileContent :: Maybe (V.Vector T.Text),
    _fileImportant :: Maybe From
} deriving Show
data AppState a = AppState {
    _coreState :: CoreState a,
    _renderState :: RenderState
} deriving Show
makeLenses ''CoreState
makeLenses ''RenderState
makeLenses ''AppState

data Requests a where
    LoadFile :: T.Text -> Requests (V.Vector T.Text)
    WhereFrom :: Heap.Box -> Requests From
deriving instance Ord (Requests a) where
    compare (WhereFrom a) (WhereFrom b) = compare (typeRep reallyUnsafePtrEq a b
    compare (LoadFile a) (LoadFile b) = compare a b
    compare (LoadFile _) _ = LT
    compare _ (LoadFile _) = GT

loadRenderState :: MonadReq Requests m => CoreState a -> m RenderState
loadRenderState cs = do
    from <- with Heap.hgeBox (lookupCurrentNode cs) $ \box -> send (WhereFrom box)
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
