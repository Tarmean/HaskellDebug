{-# LANGUAGE RecordWildCards #-}
module HeapUtils where

import GHC.HeapView
import WhereFrom
import Data.Functor ((<&>))
import qualified Data.Foldable as F
import Data.Maybe (catMaybes)
import State (HeapData)
import qualified Data.Text as T
import qualified Data.IntMap as M
import Data.List (groupBy, sortBy, intercalate, sort, group)
import Data.Function (on)
import qualified GHC.HeapView as Heap
import Control.Monad ((<=<))
import GHC.HeapView.Debug (isChar, isCons)


traverseHeapGraph :: Applicative m => (HeapGraphEntry a -> m (HeapGraphEntry b)) -> HeapGraph a -> m (HeapGraph b)
traverseHeapGraph f (HeapGraph im) = HeapGraph <$> traverse f im


traverseHeapEntry :: Applicative m => (a -> m b) -> HeapGraphEntry a -> m (HeapGraphEntry b)
traverseHeapEntry f hg = f (hgeData hg) <&> \x -> hg { hgeData = x }


childrenOf :: HeapGraphIndex -> HeapGraph t -> [HeapGraphIndex]
childrenOf i hg = case lookupHeapGraph i hg of
  Nothing -> []
  Just hge -> catMaybes $ F.toList (hgeClosure hge)

ppHeapGraphEntry :: (Int -> Maybe Int -> String) -> Int ->  HeapGraphEntry HeapData -> String
ppHeapGraphEntry pBox prec hge = case hgeClosure hge of
      ThunkClosure {..} 
        | (Just From {ipName = nam}, _) <- hgeData hge-> app $ T.unpack nam : map (pBox 10) ptrArgs ++ map show dataArgs
      FunClosure {..} 
        | (Just From {ipName = nam}, _) <- hgeData hge-> app $ T.unpack nam : map (pBox 10) ptrArgs ++ map show dataArgs
      cls -> ppClosure pBox prec cls
  where
    app [a] = a  ++ "()"
    app xs = addBraces (10 <= prec) (intercalate " " xs)

    shorten xs = if length xs > 20 then take 20 xs ++ ["(and more)"] else xs
ppHeapGraph' :: HeapGraph HeapData -> String
ppHeapGraph' (HeapGraph m) = letWrapper ++ ppRef 0 (Just heapGraphRoot)
  where
    -- All variables occuring more than once
    bindings = boundMultipleTimes (HeapGraph m) [heapGraphRoot]

    letWrapper =
        if null bindings
        then ""
        else "let " ++ intercalate "\n    " (map ppBinding bindings) ++ "\nin "

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
        map (zipWith (\j (i,c) -> (i, [c] ++ show j)) [(1::Int)..]) $
        groupBy ((==) `on` snd) $
        sortBy (compare `on` snd)
        [ (i, bindingLetter i) | i <- bindings ]

    ppVar i = ppBindingMap M.! i
    ppBinding i = ppVar i ++ " = " ++ ppEntry 0 (iToE i)

    ppEntry :: Int -> Heap.HeapGraphEntry HeapData -> String
    ppEntry prec hge
        | Just s <- isString hge = show s
        | Just l <- isList hge   = "[" ++ intercalate "," (map (ppRef 0) l) ++ "]"
        | Just bc <- disassembleBCO (fmap (hgeClosure . iToE)) (hgeClosure hge)
                                       = app ("_bco" : map (ppRef 10) (concatMap F.toList bc))
        | otherwise =  ppHeapGraphEntry ppRef prec hge
      where
        app [a] = a  ++ "()"
        app xs = addBraces (10 <= prec) (intercalate " " xs)

    ppRef :: Int -> Maybe HeapGraphIndex -> String
    ppRef _ Nothing = "..."
    ppRef prec (Just i) | i `elem` bindings = ppVar i
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
        list <- isList e
        -- We do not want to print empty lists as "" as we do not know that they
        -- are really strings.
        if (null list)
            then Nothing
            else mapM (isChar . hgeClosure <=< iToUnboundE <=< id) list

isNil :: GenClosure b -> Bool
isNil (ConstrClosure { name = "[]", dataArgs = [], ptrArgs = []}) = True
isNil _ = False

addBraces :: Bool -> String -> String
addBraces True t = "(" ++ t ++ ")"
addBraces False t = t
-- | In the given HeapMap, list all indices that are used more than once. The
-- second parameter adds external references, commonly @[heapGraphRoot]@.
boundMultipleTimes :: HeapGraph a -> [HeapGraphIndex] -> [HeapGraphIndex]
boundMultipleTimes (HeapGraph m) roots = map head $ filter (not.null) $ map tail $ group $ sort $
     roots ++ concatMap (catMaybes . allClosures . hgeClosure) (M.elems m)
