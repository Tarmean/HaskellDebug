{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
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

app :: App (AppState a) e l
app = App {
  appDraw = \s -> [rHeap (s^. coreState . heapGraph)],
  appChooseCursor = neverShowCursor,
  appHandleEvent = \e -> case e of
    VtyEvent (Inp.EvKey Inp.KEsc []) -> halt
    _ -> pure (),
  appStartEvent = return (),
  appAttrMap = const $ attrMap defAttr []
  }

printValue :: a -> IO ()
printValue a = do
  hg <- buildHeapGraph 20 () (asBox a)
  let core = CoreState hg []
  render <- loadRenderState core 
  let s = AppState core render
  _ <- defaultMain @() app s
  pure ()

-- rSource :: 
--
--
