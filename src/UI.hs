{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module UI where

import Brick
import Brick.Main
import GHC.HeapView (HeapGraph, buildHeapGraph, asBox)
import qualified GHC.HeapView as Heap
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import Lens.Micro.TH (makeLenses)
import Lens.Micro
import Lens.Micro.GHC ()
import Lens.Micro.Mtl
import Control.Monad.Trans
import Graphics.Vty.Attributes (defAttr)
import qualified Graphics.Vty.Input as Inp
import Brick.Widgets.Border
import Brick.Widgets.Core (hLimitPercent, vLimitPercent, translateBy)
import Brick.Types (availWidthL)
import qualified Brick.Types as B
import Control.Monad.Reader (withReaderT)
import GHC.Stack.CCS as Stack
import State
import HeapUtils (traverseHeapGraph, traverseHeapEntry, childrenOf, ppHeapGraph')
import WhereFrom (mkFrom, From (..), Location (..))
import AsyncRequests (runCachingT, mkCache)
import Brick.BChan (BChan, newBChan)
import Graphics.Vty (mkVty, withStyle, bold, red, withForeColor)
import Graphics.Vty.Config (defaultConfig)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe, fromJust)
import Data.List (elemIndex)
import Text.Pretty.Simple (pPrint)
import qualified Data.List as L
import Control.Applicative (Applicative(liftA2))
import qualified PrettyprinterVty as PPVty
import qualified Data.IntMap as IM
import Prettyprinter (Pretty(pretty))
import Debug.Pretty.Simple (pTraceM, pTraceShowM)
import qualified Brick as Inp
import qualified Graphics.Vty.Output.Interface as Vty
import qualified Graphics.Vty as Vty
import Control.Monad (when)


type Label = ()
-- type M t a = EventM Label (AppState t) a

-- lookupEntry :: Heap.HeapGraphIndex -> M t (Maybe (Heap.HeapGraphEntry t))
-- lookupEntry idx = use (heapGraph . to (Heap.lookupHeapGraph idx))
-- renderIdx :: Heap.HeapGraphIndex -> M t (Widget Label)
-- renderIdx idx = do
--     loadFrom idx >>= \case
--         Nothing -> pure $ str "Nothing"
-- --     from <- loadFrom idx
-- --     pure $ str $ T.unpack $ ipName from
-- retrieveCache :: FilePath -> M t (V.Vector T.Text)
-- retrieveCache fp = do
--   use (afileCache . at fp) >>= \case
--     Just t -> return t
--     Nothing -> do
--       t <- liftIO (T.readFile fp)
--       let tl = V.fromList $ T.lines  t
--       afileCache . at fp ?= tl
--       return tl
-- type Loc = T.Text
-- sourceString :: Loc -> M t T.Text
-- sourceString loc = do
--   let
--     (path, line, cols) = case T.splitOn ":" loc of
--       [a,b,c] -> (a,b,c)
--       o -> error $ "sourceString: " <> show loc <> ", " <> show o
--     (start,end) = case map readT $ T.splitOn "-" cols of
--       [a,b] -> (a,b)
--       o -> error $ "sourceString: " <> show loc <> ", " <> show o
--   theLine <- (V.! (readT line - 1)) <$> retrieveCache (T.unpack path) 
--   let theSlice = T.take (end - start+1) (T.drop (start-1) theLine)
--   pure theSlice
-- readT :: T.Text -> Int
-- readT = read . T.unpack

rHeap :: AppState -> Widget ViewPorts
rHeap s = border $ mkRelease $ clickable ViewPortHeap $ viewport ViewPortHeap Vertical $ raw $ PPVty.render (ppHeapGraph' attrMap h)
  where
      boldAttr = defAttr `withStyle` bold `withForeColor` red
      h = s^. coreState . heapGraph
      selected = s ^. coreState . activeNode . to NE.head
      attrMap = IM.singleton selected boldAttr

wrapping :: Int -> String -> String
wrapping i = unlines . chunksOf . words
  where
    addLen ls = zip (scanl (+) 0 (map length ls)) ls

    chunksOf :: [String] -> [String]
    chunksOf [] = []
    chunksOf xs = unwords (map snd l) : chunksOf (map snd r)
      where (l,r) = L.span ((<= i) . fst) (addLen xs)

rCore :: AppState -> Widget ViewPorts
rCore AppState { _renderState = RenderState { _astContent = Just vs, _fileImportant = frm}} = 
  (strWrap (show frm) <=>)  $
  border $ clickable ViewPortCore $ viewport ViewPortCore Vertical $ 
    vBox ( map (raw . PPVty.render . pretty) vs)
rCore _ = border $ strWrap "NO CORE LOADED"

showLoc :: WhereFrom.Location -> String
showLoc WhereFrom.Location { lFile, lStart = (a,b), lEnd = (c,d)}
  | a == c, b == d = pre <> show a <> ":" <> show b
  | a == c = pre <> show a <> ":" <> show b <> "-" <> show d
  | otherwise = pre <> "(" <> show a <> "," <> show b <> ")-(" <> show c <> "," <> show d <> ")"
  where pre = T.unpack lFile <> ":"

data ViewPorts = ViewPortFile | ViewPortHeap | ViewPortCore
  deriving (Eq, Ord, Show)
rFile :: AppState -> Widget ViewPorts
rFile AppState { _renderState = RenderState { _fileContent = Just vs, _fileImportant = Just WhereFrom.From {WhereFrom.ipLoc = Just loc } }}
  = (str (showLoc loc) <=>) $ border $
     clickable ViewPortFile $ viewport ViewPortFile Vertical $
      (strWrap  $ unlines $ map T.unpack $ V.toList vs)
rFile s = border $ str ("? " <> show active <> ":\n" <> frm <> "\n" <> wrapping 70 snode)
  where
     active = s ^. coreState . activeNode  . to NE.head
     node = s ^. coreState . heapGraph . to (Heap.lookupHeapGraph active)
     snode = case node of
        Just hge -> Heap.ppClosure (\_ a -> sRef a) 10 (Heap.hgeClosure hge)
        _ -> show node
     frm = maybe "" show (s ^. renderState . fileImportant)
     sRef Nothing = "..."
     sRef (Just i) = "*" <> show i
rWhoCreated :: AppState -> Widget l
rWhoCreated s = case node of
    Nothing -> border (str "No who Created")
    Just xs -> border $  vBox $ map str $ xs
  where
     active = s ^. coreState . activeNode  . to NE.head
     node = s ^? coreState . heapGraph . to (Heap.lookupHeapGraph active) . _Just . to Heap.hgeData . to hwhoCreated

slicing :: Int -> Int -> V.Vector a -> V.Vector a
slicing l r = V.take (r-l+3) . V.drop (l-2) 

app :: App AppState e ViewPorts
app = App {
  appDraw = \s -> [hLimitPercent 70 (rHeap s) <+> (vLimitPercent 60 (rFile s) <=> rWhoCreated s <=> (rCore s))],
  appChooseCursor = neverShowCursor,
  appHandleEvent = \e -> do
     case e of
      VtyEvent (Inp.EvKey (Inp.KChar 'j') []) -> moveToChild
      VtyEvent (Inp.EvKey (Inp.KChar 'l') []) -> moveToSibling 1
      VtyEvent (Inp.EvKey (Inp.KChar 'h') []) -> moveToSibling (-1)
      VtyEvent (Inp.EvKey (Inp.KChar 'k') []) -> moveToParent
      VtyEvent (Inp.EvKey (Inp.KChar 'r') []) -> forceAndReload
      VtyEvent (Inp.EvKey Inp.KEsc []) -> halt
      Inp.MouseDown ViewPortFile Inp.BScrollDown _ _ -> vScrollBy (viewportScroll ViewPortFile) 5
      Inp.MouseDown ViewPortFile Inp.BScrollUp _ _ -> vScrollBy (viewportScroll ViewPortFile) (-5)
      Inp.MouseDown ViewPortHeap Inp.BScrollDown _ _ -> vScrollBy (viewportScroll ViewPortHeap) 5
      Inp.MouseDown ViewPortHeap Inp.BScrollUp _ _ -> vScrollBy (viewportScroll ViewPortHeap) (-5)
      Inp.MouseDown ViewPortCore Inp.BScrollDown _ _ -> vScrollBy (viewportScroll ViewPortCore) 5
      Inp.MouseDown ViewPortCore Inp.BScrollUp _ _ -> vScrollBy (viewportScroll ViewPortCore) (-5)
      _ -> pure ()
     s <- get
     let os = s
     s <- rebuildState s
     setScrolls (os ^. renderState) (s ^. renderState)
     put s,
  appStartEvent = do
      s <- get
      setScrolls (RenderState Nothing Nothing Nothing) (s ^. renderState),
  appAttrMap = const $ attrMap defAttr []
  }
  where
    setScrolls os s = do
      when (os ^. fileImportant /= s ^. fileImportant) $ do
       case s ^. fileImportant of
           Just (WhereFrom.From { ipLoc = Just WhereFrom.Location { lStart = (a,b)}}) -> do
             setTop (viewportScroll ViewPortFile) a 
           _ -> pure ()
mkRelease p =
    case hSize p of
        Fixed -> Widget Greedy (hSize p) $
                        withReaderT (availWidthL .~  10000) (render p)
        Greedy -> p
whenIn :: Eq n => Int -> Int -> n -> EventM n s () -> EventM n s ()
whenIn col row n body = do
    lookupExtent n >>= \case
      Just extent | clickedExtent (col,row) extent -> body
      _ -> pure ()
moveToParent :: EventM l AppState ()
moveToParent = do
  s <- get
  case (s ^. coreState . activeNode) of
       _ NE.:| (y : ys) -> coreState . activeNode .= y NE.:| ys
       _ -> error "no"
moveToChild :: EventM l AppState ()
moveToChild = do
  s <- get
  let candidates = childrenOf (NE.head (s ^. coreState . activeNode)) (s^. coreState . heapGraph)
  case candidates of
       [] -> pure ()
       x:_ -> coreState .  activeNode .=  x `NE.cons` (s ^. coreState . activeNode)

forceAndReload :: EventM l AppState ()
forceAndReload = do
      s <- get
      let hge = s ^. coreState . heapGraph . to (Heap.lookupHeapGraph (NE.head (s ^. coreState . activeNode)))
      case hge of
          Nothing -> pure ()
          Just (Heap.HeapGraphEntry { Heap.hgeBox = (Heap.Box b)}) -> do
               b `seq` pure ()
               let rootBox = Heap.hgeBox $ fromJust $ Heap.lookupHeapGraph 0 (s ^. coreState . heapGraph)
               cr <- liftIO (mkCoreState rootBox)
               coreState . heapGraph .= cr


moveToSibling :: Int -> EventM l AppState ()
moveToSibling i = do
  s <- get
  case (s ^. coreState . activeNode) of
    c NE.:| y:xs -> do
      let candidates = childrenOf y (s^. coreState . heapGraph)
          pos  = fromMaybe 0 $ elemIndex c candidates
          pos'  = pos + i
      if pos' >= 0 && pos' < length candidates
        then coreState .  activeNode .=  (candidates !! pos') `NE.cons` (y NE.:| xs)
        else pure ()
    _ -> pure ()
defaultMainChan :: (Ord n)
            => App s e n
            -- ^ The application.
            -> s
            -- ^ The initial application state.
            -> BChan e
            -- ^ The event channel.
            -> IO s
defaultMainChan app st chan = do
    let builder = mkVty defaultConfig
    initialVty <- builder
    let output = Vty.outputIface initialVty
    when (Vty.supportsMode output Vty.Mouse) $
        liftIO $ Vty.setMode output Vty.Mouse True
    customMain initialVty builder (Just chan) app st

mkCoreState :: Heap.Box -> IO (HeapGraph HeapData)
mkCoreState b = do
  hg <- liftIO (buildHeapGraph 10 () b)
  let mk1 he = liftA2 HeapData (mkFrom (Heap.hgeBox he)) (case Heap.hgeBox he of Heap.Box e -> whoCreated e) 
  hg <- liftIO $ traverseHeapGraph (\he -> mk1 he <&> \dat -> he {Heap.hgeData = dat}) hg
  -- pTraceShowM hg
  pure hg
printValue :: a -> IO ()
printValue a = do
  cache <- mkCache runRequest (\_ -> pure ())
  hg <- mkCoreState (Heap.asBox a)
  let core = CoreState hg (0 NE.:| [])
  render <- loadRenderState core cache
  let s = AppState core render cache
  -- print (core,render)
  print $ ppHeapGraph' mempty (core ^. heapGraph)
  chan <- liftIO (newBChan 8)
  _ <- liftIO $ defaultMainChan @ViewPorts app s chan
  pure ()

-- rSource :: 
--
--
