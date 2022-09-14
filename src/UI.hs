{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module UI where

import Brick
import Brick.Main
import GHC.HeapView (HeapGraph, ppHeapGraph, buildHeapGraph, asBox)
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
import GHC.Stack.CCS as Stack
import State
import HeapUtils (traverseHeapGraph, traverseHeapEntry, childrenOf)
import WhereFrom (mkFrom, From (From, ipLoc), Location (..))
import AsyncRequests (runCachingT, mkCache)
import Brick.BChan (BChan, newBChan)
import Graphics.Vty (mkVty)
import Graphics.Vty.Config (defaultConfig)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.List (elemIndex)
import Text.Pretty.Simple (pPrint)
import qualified Data.List as L


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

rHeap :: HeapGraph a -> Widget l
rHeap h = border $ str (ppHeapGraph h)

wrapping :: Int -> String -> String
wrapping i = unlines . chunksOf . words
  where
    addLen ls = zip (scanl (+) 0 (map length ls)) ls

    chunksOf :: [String] -> [String]
    chunksOf [] = []
    chunksOf xs = unwords (map snd l) : chunksOf (map snd r)
      where (l,r) = L.span ((<= i) . fst) (addLen xs)

rFile :: AppState -> Widget l
rFile AppState { _renderState = RenderState { _fileContent = Just vs, _fileImportant = Just From{ipLoc = Just WhereFrom.Location{lStart=(l,_), lEnd=(r,_)}}} } = border $  vBox $ map (str . T.unpack) . V.toList $  slicing l r vs
rFile s = border $ str ("? " <> show active <> ":\n" <> wrapping 70 (show node))
  where
     active = s ^. coreState . activeNode  . to NE.head
     node = s ^. coreState . heapGraph . to (Heap.lookupHeapGraph active)

slicing :: Int -> Int -> V.Vector a -> V.Vector a
slicing l r = V.take (r-l+3) . V.drop (l-2) 

app :: App AppState e l
app = App {
  appDraw = \s -> [rHeap (s^. coreState . heapGraph) <=> rFile s],
  appChooseCursor = neverShowCursor,
  appHandleEvent = \e -> do
     case e of
      VtyEvent (Inp.EvKey (Inp.KChar 'l') []) -> moveToChild
      VtyEvent (Inp.EvKey (Inp.KChar 'j') []) -> moveToSibling 1
      VtyEvent (Inp.EvKey (Inp.KChar 'k') []) -> moveToSibling (-1)
      VtyEvent (Inp.EvKey (Inp.KChar 'h') []) -> moveToParent
      VtyEvent (Inp.EvKey Inp.KEsc []) -> halt
      _ -> pure ()
     s <- get
     s <- rebuildState s
     put s,
  appStartEvent = return (),
  appAttrMap = const $ attrMap defAttr []
  }

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
    customMain initialVty builder (Just chan) app st
printValue :: a -> IO ()
printValue a = do
  cache <- mkCache runRequest (\_ -> pure ())
  hg <- liftIO (buildHeapGraph 10 () (asBox a))
  hg <- liftIO $ traverseHeapGraph (\he ->  mkFrom (Heap.hgeBox he) <&> \dat -> he {Heap.hgeData = dat}) hg
  pPrint hg
  let core = CoreState hg (0 NE.:| [])
  render <- loadRenderState core cache
  let s = AppState core render cache
  chan <- liftIO (newBChan 8)
  _ <- liftIO $ defaultMainChan @() app s chan
  pure ()

-- rSource :: 
--
--
