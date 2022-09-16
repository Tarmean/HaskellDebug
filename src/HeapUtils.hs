{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}
module HeapUtils where

import GHC.HeapView
import WhereFrom
import Data.Functor ((<&>))
import qualified Data.Foldable as F
import Data.Maybe (catMaybes)
import State (HeapData)
import qualified Data.Text as T
import qualified Data.IntMap as M
import qualified Data.List as L
import Data.Function (on)
import qualified GHC.HeapView as Heap
import Control.Monad ((<=<))
import GHC.HeapView.Debug (isChar, isCons)
import Prettyprinter
import Graphics.Vty.Attributes (Attr)
import Data.String
import Data.Functor.Identity
import OOP
import Control.Monad.State


traverseHeapGraph :: Applicative m => (HeapGraphEntry a -> m (HeapGraphEntry b)) -> HeapGraph a -> m (HeapGraph b)
traverseHeapGraph f (HeapGraph im) = HeapGraph <$> traverse f im


traverseHeapEntry :: Applicative m => (a -> m b) -> HeapGraphEntry a -> m (HeapGraphEntry b)
traverseHeapEntry f hg = f (hgeData hg) <&> \x -> hg { hgeData = x }


childrenOf :: HeapGraphIndex -> HeapGraph t -> [HeapGraphIndex]
childrenOf i hg = case lookupHeapGraph i hg of
  Nothing -> []
  Just hge -> catMaybes $ F.toList (hgeClosure hge)

ppHeapGraphEntry :: (Int -> Maybe Int -> Doc Attr) -> Int ->  HeapGraphEntry HeapData -> Doc Attr
ppHeapGraphEntry pBox prec hge = case hgeClosure hge of
      ThunkClosure {..} 
        | (Just From {ipName = nam}, _) <- hgeData hge-> app $ pretty nam : map (pBox 10) ptrArgs <> map pretty dataArgs
      FunClosure {..} 
        | (Just From {ipName = nam}, _) <- hgeData hge-> app $ pretty nam : map (pBox 10) ptrArgs <> map pretty dataArgs
      cls -> ppClosureDoc pBox prec cls
  where
    app [a] = a  <> "()"
    app xs = addBraces (10 <= prec) (align $ sep xs)

    shorten xs = if length xs > 20 then take 20 xs <> ["(and more)"] else xs

ppHeapGraph' :: HeapGraph HeapData -> Doc Attr
ppHeapGraph' (HeapGraph m) = letWrapper <> ppRef 0 (Just heapGraphRoot)
  where
    -- All variables occuring more than once
    bindings = boundMultipleTimes (HeapGraph m) [heapGraphRoot]

    letWrapper =
        if null bindings
        then ""
        else "let " <> align (hsep (map ppBinding bindings)) <> line <> "in "

    bindingLetter i = case hgeClosure (iToE i) of
        ThunkClosure {} -> 't'
        SelectorClosure {} -> 't'
        APClosure {} -> 't'
        PAPClosure {} -> 'f'
        BCOClosure {} -> 't'
        FunClosure {} -> 'f'
        _ -> 'x'

    ppBindingMap = M.fromList $
        concat $
        map (zipWith (\j (i,c) -> (i, [c] <> show j)) [(1::Int)..]) $
        L.groupBy ((==) `on` snd) $
        L.sortBy (compare `on` snd)
        [ (i, bindingLetter i) | i <- bindings ]

    ppVar i = ppBindingMap M.! i
    ppBinding :: Int -> Doc Attr
    ppBinding i = pretty (ppVar i) <> " = " <> ppEntry 0 (iToE i)


    ppEntry :: Int -> Heap.HeapGraphEntry HeapData -> Doc Attr
    ppEntry prec hge
        | Just s <- isString hge = pretty $ show s
        | Just l <- isList hge   = list (map (ppRef 0) l)
        | Just bc <- disassembleBCO (fmap (hgeClosure . iToE)) (hgeClosure hge)
                                       = app ("_bco" : map (ppRef 10) (concatMap F.toList bc))
        | otherwise =  ppHeapGraphEntry ppRef prec hge
      where
        app [a] = a  <> "()"
        app xs = addBraces (10 <= prec) (align $ sep xs)

    ppRef :: Int -> Maybe HeapGraphIndex -> Doc Attr
    ppRef _ Nothing = "..."
    ppRef prec (Just i) | i `elem` bindings = pretty (ppVar i)
                        | otherwise = ppEntry prec (iToE i)
    iToE i = m M.! i

    iToUnboundE i = if i `elem` bindings then Nothing else M.lookup i m

    isList :: HeapGraphEntry a -> Maybe ([Maybe HeapGraphIndex])
    isList hge =
        if isNil (hgeClosure hge)
          then return []
          else do
            (h,t) <- isCons (hgeClosure hge)
            ti <- t
            e <- iToUnboundE ti
            t' <- isList e
            return $ (:) h t'

    isString :: HeapGraphEntry a -> Maybe String
    isString e = do
        ls <- isList e
        -- We do not want to print empty lists as "" as we do not know that they
        -- are really strings.
        if (null ls)
            then Nothing
            else mapM (isChar . hgeClosure <=< iToUnboundE <=< id) ls

class Applicative m => PrintClosure r t f m where
      pClosImpl :: (?printBase :: Proxy# r, ?printLocal :: Proxy# t) => Int -> f -> m (Doc Attr)
instance (PrintClosure r r b m) => PrintClosure r Base (GenClosure b) m where
      pClosImpl  i v = ppClosureDoc' (\x y -> self (pClos x y)) i v
instance (PrintClosure r r (HeapGraphEntry t) m, MonadState (HeapGraph t) m) => PrintClosure r Base HeapGraphIndex m where
  pClosImpl = do
        undefined -- ppHeapGraphEntry

pClos :: forall t r f x m. (?printBase :: Proxy# r, PrintClosure r x f m) => Int -> f -> Dispatch t r x (m (Doc Attr))
pClos a b = Dispatch (let ?printLocal = proxy# :: Proxy# x in pClosImpl a b)

isNil :: GenClosure b -> Bool
isNil (ConstrClosure { name = "[]", dataArgs = [], ptrArgs = []}) = True
isNil _ = False

addBraces :: (Monoid s, IsString s) => Bool -> s -> s
addBraces True t = "(" <> t <> ")"
addBraces False t = t
-- | In the given HeapMap, list all indices that are used more than once. The
-- second parameter adds external references, commonly @[heapGraphRoot]@.
boundMultipleTimes :: HeapGraph a -> [HeapGraphIndex] -> [HeapGraphIndex]
boundMultipleTimes (HeapGraph m) roots = map head $ filter (not.null) $ map tail $ L.group $ L.sort $
     roots <> concatMap (catMaybes . allClosures . hgeClosure) (M.elems m)

ppClosureDoc :: (Int -> t -> Doc Attr) -> Int -> GenClosure t -> Doc Attr
ppClosureDoc f b c = runIdentity (ppClosureDoc' (\x y -> Identity (f x y)) b c)

ppClosureDoc' :: Applicative m => (Int -> b -> m (Doc Attr)) -> Int -> GenClosure b -> m (Doc Attr)
ppClosureDoc' showBox prec c = case c of
    _ | Just ch <- isChar c -> pure $ app ["C#", pretty ch]
    _ | Just (h,t) <- isCons c -> addBraces (5 <= prec) <$> do
        l <- showBox 5 h 
        r <- showBox 4 t
        pure $ l <> " : " <> r 
    _ | Just vs <- isTup c ->
        fmap tupled (traverse (showBox 1) vs)
    ConstrClosure {..} -> do
      ptrs <- traverse (showBox 10) ptrArgs
      pure $ app $ pretty name : ptrs <> map pretty dataArgs
    ThunkClosure {..} -> do
       
        ptrs <- traverse (showBox 10) ptrArgs
        pure $ app $ "_thunk" : ptrs <> map pretty dataArgs
    SelectorClosure {..} -> appBox "_sel" selectee
    IndClosure {..} -> appBox "_ind" indirectee
    BlackholeClosure {..} -> appBox "_bh" indirectee
    APClosure {..} -> fmap app (traverse (showBox 10) (fun : payload))
    PAPClosure {..} -> fmap app (traverse (showBox 10) (fun : payload))
    APStackClosure {..} -> fmap app (traverse (showBox 10) (fun : payload))
    BCOClosure {..} -> appBox "_bco" bcoptrs
    ArrWordsClosure {..} -> pure $ app
        ["toArray", "("<>pretty (length arrWords) <> " words)", sep $ punctuate comma (shorten (map pretty arrWords)) ]
    MutArrClosure {..} -> do
        payload <- traverse (showBox 10) mccPayload
        --["toMutArray", "("<>show (length mccPayload) <> " ptrs)",  L.intercalate "," (shorten (map (showBox 10) mccPayload))]
        pure $ list (shorten payload)
    MutVarClosure {..} -> appBox "_mutVar" var
    MVarClosure {..} ->  appBox "MVar" value
    FunClosure {..} -> do
        ptrs <- traverse (showBox 0) ptrArgs
        pure $ "_fun" <> braceize (ptrs <> map pretty dataArgs)
    BlockingQueueClosure {..} -> pure "_blockingQueue"
    IntClosure {..} -> pure $ app ["Int", pretty intVal]
    WordClosure {..} -> pure $ app
        ["Word", pretty wordVal]
    Int64Closure {..} -> pure $ app
        ["Int64", pretty int64Val]
    Word64Closure {..} -> pure $ app
        ["Word64", pretty word64Val]
    AddrClosure {..} -> pure $ app
        ["Addr", pretty addrVal]
    FloatClosure {..} -> pure $ app
        ["Float", pretty floatVal]
    DoubleClosure {..} -> pure $ app
        ["Double", pretty doubleVal]
    OtherClosure {..} ->
        pure "_other"
    UnsupportedClosure {..} ->
        pure "_unsupported closure"
    -- copy-pasta'd from MutArrClosure:
    SmallMutArrClosure {..} -> do
        --["toMutArray", "("<>show (length mccPayload) <> " ptrs)",  intercalate "," (shorten (map (showBox 10) mccPayload))]
        payload <- traverse (showBox 10) mccPayload
        pure $ list $ (shorten payload)
    WeakClosure {..} -> pure "_weak"
    IOPortClosure {} -> pure "_ioPortClosure"
    TSOClosure {} -> pure "_tsoClosure"
    StackClosure{} -> pure "_stackClosure"
  where
    appBox str sel = do
        s <- showBox 10 sel
        pure $ app [str, s]
    app [a] = a  <> "()"
    app xs = addBraces (10 <= prec) (sep xs)

    shorten xs = if length xs > 20 then take 20 xs <> ["(and more)"] else xs

braceize :: [Doc a] -> Doc a
braceize [] = ""
braceize xs = braces $  hsep $ punctuate comma xs 
isTup :: GenClosure b -> Maybe [b]
isTup (ConstrClosure { dataArgs = [], ..}) =
    if length name >= 3 &&
       head name == '(' && last name == ')' &&
       all (==',') (tail (init name))
    then Just ptrArgs else Nothing
isTup _ = Nothing