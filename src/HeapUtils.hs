{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module HeapUtils where

import GHC.HeapView
import WhereFrom
import Data.Functor ((<&>))
import qualified Data.Foldable as F
import Data.Maybe (catMaybes, fromJust)
import State (HeapData)
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.List as L
import GHC.HeapView.Debug (isChar, isCons)
import Prettyprinter
import Graphics.Vty.Attributes (Attr)
import Data.String
import Data.Functor.Identity
import OOP
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Applicative (Alternative(..))


traverseHeapGraph :: Applicative m => (HeapGraphEntry a -> m (HeapGraphEntry b)) -> HeapGraph a -> m (HeapGraph b)
traverseHeapGraph f (HeapGraph im) = HeapGraph <$> traverse f im


traverseHeapEntry :: Applicative m => (a -> m b) -> HeapGraphEntry a -> m (HeapGraphEntry b)
traverseHeapEntry f hg = f (hgeData hg) <&> \x -> hg { hgeData = x }


childrenOf :: HeapGraphIndex -> HeapGraph t -> [HeapGraphIndex]
childrenOf i hg = case lookupHeapGraph i hg of
  Nothing -> []
  Just hge -> catMaybes $ F.toList (hgeClosure hge)

-- Use data from InfoTableMap to give better names for thunks and functions
data NamedThunks f
instance {-# OVERLAPS #-} (PrintClosure r f b m) => PrintClosure r (NamedThunks f) b m where
    pClosImpl p f = super (pClos p f)
instance (PrintClosure r r (Maybe HeapGraphIndex) m, PrintClosure r f (HeapGraphEntry HeapData) m) =>  PrintClosure r (NamedThunks f) (HeapGraphEntry HeapData) m where
    pClosImpl prec hge = case hgeClosure hge of
      ThunkClosure {..} 
        | (Just From {ipName = nam}, _) <- hgeData hge-> do
            ptrs <-  traverse (self . pClos 10) ptrArgs
            pure $ app $ pretty nam :ptrs <> map pretty dataArgs
      FunClosure {..} 
        | (Just From {ipName = nam}, _) <- hgeData hge-> do
            ptrs <- traverse (self . pClos 10) ptrArgs
            pure $ app $ pretty nam : ptrs <> map pretty dataArgs
      _ -> super (pClos prec hge)
     where
        app [a] = a  <> "()"
        app xs = addBraces (10 <= prec) (align $ sep xs)
instance (PrintClosure r r (GenClosure (Maybe HeapGraphIndex)) m) =>  PrintClosure r Base (HeapGraphEntry HeapData) m where
     pClosImpl prec hge = self (pClos prec (hgeClosure hge))


instance (MonadHeapPrint b m, PrintClosure r r (HeapGraphEntry b) m) => PrintClosure r Base (Maybe HeapGraphIndex) m where
    pClosImpl _ Nothing = pure "..."
    pClosImpl r (Just i) =  lookupHeapRep i >>= \case
        Left l -> pure $ pretty l
        Right e -> self $ pClos r e

-- Specialized printing for lists, strings, and bytecode objects
data PrettyListLiterals f
instance {-# OVERLAPS #-} (PrintClosure r f b m) => PrintClosure r (PrettyListLiterals f) b m where
    pClosImpl p f = super (pClos p f)
instance (MonadHeapPrint b m, PrintClosure r r (Maybe HeapGraphIndex) m, PrintClosure r f (HeapGraphEntry b) m) => PrintClosure r (PrettyListLiterals f) (HeapGraphEntry b) m where
    pClosImpl prec hge0 = do
        runMaybeT (isString hge0) >>= \case
            Just s -> pure $ pretty $ show s
            Nothing -> 
                runMaybeT (isList hge0) >>= \case
                Just l -> list <$> (traverse (self . pClos 0) l)
                Nothing -> do
                    HeapGraph s <- getHeapContent
                    case disassembleBCO (fmap (hgeClosure . (s IM.!))) (hgeClosure hge0) of
                        Just bc -> do
                            o <- traverse (self . pClos 10) (concatMap F.toList bc)
                            pure $ app ("_bco" : o) 
                        Nothing -> super $ pClos prec hge0
        where
            liftMay :: Maybe a -> MaybeT m a
            liftMay Nothing = empty
            liftMay (Just a) = pure a

            isList :: HeapGraphEntry a -> MaybeT m [Maybe HeapGraphIndex]
            isList hge =
                if isNil (hgeClosure hge)
                then pure []
                else do
                    (h,t) <- liftMay $ isCons (hgeClosure hge)
                    ti <- liftMay t
                    e <- MaybeT $ lookupHeapUnbound ti
                    t' <- isList e
                    return $ (:) h t'

            isString :: HeapGraphEntry a -> MaybeT m String
            isString e = do
                ls <- isList e
                -- We do not want to print empty lists as "" as we do not know that they
                -- are really strings.
                if (null ls)
                    then empty
                    else mapM (liftMay . isChar . hgeClosure <=< MaybeT . lookupHeapUnbound <=< liftMay) ls
            app [a] = a  <> "()"
            app xs = addBraces (10 <= prec) (sep xs)

data Elide t a
instance Applicative m => PrintClosure r (Elide t f) t m where
    pClosImpl _ _ = pure "..."
instance {-# OVERLAPS #-} (PrintClosure r f s m) => PrintClosure r (Elide t f) s m where
    pClosImpl prec c = super (pClos prec c)
class Applicative m => PrintClosure r t f m where
      pClosImpl :: (?printBase :: Proxy# r, ?printLocal :: Proxy# t) => Int -> f -> m (Doc Attr)
instance (PrintClosure r r b m) => PrintClosure r Base (GenClosure b) m where
    pClosImpl  prec c = case c of
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
        BlockingQueueClosure {} -> pure "_blockingQueue"
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
        showBox i' v' = self (pClos i' v')
        appBox str sel = do
            s <- showBox 10 sel
            pure $ app [str, s]
        app [a] = a  <> "()"
        app xs = addBraces (10 <= prec) (sep xs)

        shorten xs = if length xs > 20 then take 20 xs <> ["(and more)"] else xs

type Printer = Elide (Maybe From, [String]) (PrettyListLiterals (NamedThunks Base))
printClosure :: forall r f m. (PrintClosure r r f m) => Int -> f -> m (Doc Attr)
printClosure prec f = let ?printBase = proxy# @r in self (pClos prec f)

ppHeapGraph'' :: forall x t m v. (PrintClosure x x v m) => v -> m (Doc Attr)
ppHeapGraph'' a = let ?printBase = proxy# @x; ?printLocal = proxy# @x in self (pClos 10 a)
ppHeapGraph' :: a
ppHeapGraph' = undefined

pClos :: forall t r f x m. (?printBase :: Proxy# r, PrintClosure r x f m) => Int -> f -> Dispatch t r x (m (Doc Attr))
pClos a b = Dispatch (let ?printLocal = proxy# :: Proxy# x in pClosImpl a b)

isNil :: GenClosure b -> Bool
isNil (ConstrClosure { name = "[]", dataArgs = [], ptrArgs = []}) = True
isNil _ = False

addBraces :: (Monoid s, IsString s) => Bool -> s -> s
addBraces True t = "(" <> t <> ")"
addBraces False t = t

ppClosureDoc :: Int -> GenClosure HeapData -> Doc Attr
ppClosureDoc b c = runIdentity (printClosure @Printer b c)


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

class Monad m => MonadHeapPrint t m | m -> t where
    getHeapBindings :: m (IM.IntMap String)
    getHeapContent :: m (HeapGraph t)
lookupBindingLabel :: MonadHeapPrint t m => HeapGraphIndex -> m (Maybe String)
lookupBindingLabel i = IM.lookup i <$> getHeapBindings
lookupHeapRep :: MonadHeapPrint t m => HeapGraphIndex -> m (Either String (HeapGraphEntry t))
lookupHeapRep i = lookupBindingLabel i >>= \case
    Nothing -> Right <$> lookupHeapClosure i
    Just o -> pure $ Left o
lookupHeapClosure :: MonadHeapPrint t m => HeapGraphIndex -> m (HeapGraphEntry t)
lookupHeapClosure i = fromJust . lookupHeapGraph i <$> getHeapContent
lookupHeapUnbound :: MonadHeapPrint t m => HeapGraphIndex -> m (Maybe (HeapGraphEntry t))
lookupHeapUnbound i = do
    heapIsBound i >>= \case
      True -> pure Nothing
      False -> lookupHeapGraph i <$> getHeapContent
heapIsBound :: MonadHeapPrint t m => HeapGraphIndex -> m Bool
heapIsBound i = IM.member i <$> getHeapBindings
mkBindingMap :: HeapGraph t -> IM.IntMap String
mkBindingMap (HeapGraph m) = ppBindingMap
  where
    bindings = boundMultipleTimes (HeapGraph m) [heapGraphRoot]
    -- | In the given HeapMap, list all indices that are used more than once. The
    -- second parameter adds external references, commonly @[heapGraphRoot]@.
    boundMultipleTimes :: HeapGraph a -> [HeapGraphIndex] -> [HeapGraphIndex]
    boundMultipleTimes (HeapGraph m) roots = map head $ filter (not.null) $ map tail $ L.group $ L.sort $
        roots <> concatMap (catMaybes . allClosures . hgeClosure) (IM.elems m)

    bindingLetter i = case hgeClosure (iToE i) of
        ThunkClosure {} -> 't'
        SelectorClosure {} -> 't'
        APClosure {} -> 't'
        PAPClosure {} -> 'f'
        BCOClosure {} -> 't'
        FunClosure {} -> 'f'
        _ -> 'x'

    ppBindingMap = IM.fromList
        [ (v, k : show idx)
        | (k,vs) <- M.toList groupedBindings
        , (v, idx) <- zip vs [1..]
        ]
    groupedBindings = M.fromListWith (<>) [ (bindingLetter i, [i]) | i <- bindings ]
    iToE i = m IM.! i