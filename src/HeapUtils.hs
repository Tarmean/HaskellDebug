{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -ddump-simpl -dsuppress-all -dsuppress-uniques -O0 #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
module HeapUtils where



import GHC.HeapView
import WhereFrom
import Data.Functor ((<&>))
import qualified Data.Foldable as F
import Data.Maybe (catMaybes, fromJust, isJust)
import State (HeapData (..))
import qualified Data.Map as M
import qualified Data.IntMap as IM
import GHC.HeapView.Debug (isChar, isCons)
import Prettyprinter
import Graphics.Vty.Attributes (Attr)
import Data.String
import Data.Functor.Identity
import OOP
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Applicative (Alternative(..))
import Control.Monad.Reader
import qualified Data.IntSet as IS
import GHC.Stack (HasCallStack)
import Debug.Trace
import Data.Containers.ListUtils (nubOrd)
import qualified Data.Text as T
import qualified Data.Text.Internal as TI
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.Text.Array as A


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
        | Just nam <- toName (hgeData hge) -> do
            ptrs <-  traverse (self . pClos 10) ptrArgs
            pure $ app prec $ pretty nam :ptrs <> map pretty dataArgs
      FunClosure {..} 
        | Just nam <- toName (hgeData hge) -> do
            ptrs <- traverse (self . pClos 10) ptrArgs
            pure $ app prec $ pretty nam : ptrs <> map pretty dataArgs
      _ -> super (pClos prec hge)
      where
        toName HeapData {hsourceLoc = Just From {ipName = nam, ipLabel =lab}}
          | not (T.null lab) = Just lab
          | otherwise = Just nam
        toName _ = Nothing
        
instance (PrintClosure r r (GenClosure (Maybe HeapGraphIndex)) m) =>  PrintClosure r Base (HeapGraphEntry HeapData) m where
     pClosImpl prec hge = self (pClos prec (hgeClosure hge))


instance (PrintClosure r r x m) => PrintClosure r Base (Maybe x) m where
    pClosImpl _ Nothing = pure "..."
    pClosImpl r (Just i) =  self (pClos r i)
instance (MonadHeapPrint b m, PrintClosure r r (HeapGraphEntry b) m) => PrintClosure r Base HeapGraphIndex m where
    pClosImpl r i =  lookupHeapRep i >>= \case
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
                    case disassembleBCO (fmap (hgeClosure . (s !!!))) (hgeClosure hge0) of
                        Just bc -> do
                            o <- traverse (self . pClos 10) (concatMap F.toList bc)
                            pure $ app prec ("_bco" : o) 
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

data PrettyText f
instance {-# OVERLAPS #-} (PrintClosure r f b m) => PrintClosure r (PrettyText f) b m where
    pClosImpl p f = super (pClos p f)
instance (MonadHeapPrint b m, PrintClosure r f (HeapGraphEntry b) m) => PrintClosure r (PrettyText f) (HeapGraphEntry b) m where
    pClosImpl prec hge0 = do
        runMaybeT (isText hge0) >>= \case
            Just s -> pure $ pretty $ show s
            Nothing -> super $ pClos prec hge0
        where
            liftMay :: Maybe a -> MaybeT m a
            liftMay Nothing = empty
            liftMay (Just a) = pure a

            isText :: HeapGraphEntry a -> MaybeT m T.Text
            isText hge = do
                (len, offs, Just p) <- liftMay $ isTextCons (hgeClosure hge)
                HeapGraphEntry { hgeBox = b, hgeClosure = ArrWordsClosure {}} <- MaybeT $ lookupHeapUnbound p
                return $ TI.Text (unsafeCoerce b) (fromIntegral len) (fromIntegral offs) 

            isTextCons :: GenClosure (Maybe HeapGraphIndex) -> Maybe (Word, Word, Maybe HeapGraphIndex)
            isTextCons (ConstrClosure { name = "Text", dataArgs = [len,offs], ptrArgs = [p]}) = Just (len, offs, p)
            isTextCons _ = Nothing
                

data Elide t a
instance Applicative m => PrintClosure r (Elide t f) t m where
    pClosImpl _ _ = pure "..."
instance {-# OVERLAPS #-} (PrintClosure r f s m) => PrintClosure r (Elide t f) s m where
    pClosImpl prec c = super (pClos prec c)
class Applicative m => PrintClosure r t f m where
      pClosImpl :: (?printBase :: Proxy# r, ?printLocal :: Proxy# t) => Int -> f -> m (Doc Attr)
instance (PrintClosure r r b m) => PrintClosure r Base (GenClosure b) m where
    pClosImpl  prec c = case c of
        _ | Just ch <- isChar c -> pure $ app prec ["C#", pretty ch]
        _ | Just (h,t) <- isCons c -> addBraces (5 <= prec) <$> do
            l <- showBox 5 h 
            r <- showBox 4 t
            pure $ l <> " : " <> r 
        _ | Just vs <- isTup c ->
            fmap tupled (traverse (showBox 1) vs)
        ConstrClosure {..} -> do
            ptrs <- traverse (showBox 0) ptrArgs
            pure $ pretty name <+> braces (align $ sep $ punctuate comma (ptrs <> map pretty dataArgs))
        ThunkClosure {..} -> do
            ptrs <- traverse (showBox 10) ptrArgs
            pure $ app prec $ "_thunk" : ptrs <> map pretty dataArgs
        SelectorClosure {..} -> appBox "_sel" selectee
        IndClosure {..} -> appBox "_ind" indirectee
        BlackholeClosure {..} -> appBox "_bh" indirectee
        APClosure {..} -> fmap (app prec) (traverse (showBox 10) (fun : payload))
        PAPClosure {..} -> fmap (app prec) (traverse (showBox 10) (fun : payload))
        APStackClosure {..} -> fmap (app prec) (traverse (showBox 10) (fun : payload))
        BCOClosure {..} -> appBox "_bco" bcoptrs
        ArrWordsClosure {..} -> pure $ app prec
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
        IntClosure {..} -> pure $ app prec ["Int", pretty intVal]
        WordClosure {..} -> pure $ app prec
            ["Word", pretty wordVal]
        Int64Closure {..} -> pure $ app prec
            ["Int64", pretty int64Val]
        Word64Closure {..} -> pure $ app prec
            ["Word64", pretty word64Val]
        AddrClosure {..} -> pure $ app prec
            ["Addr", pretty addrVal]
        FloatClosure {..} -> pure $ app prec
            ["Float", pretty floatVal]
        DoubleClosure {..} -> pure $ app prec
            ["Double", pretty doubleVal]
        OtherClosure {} ->
            pure "_other"
        UnsupportedClosure {} ->
            pure "_unsupported closure"
        -- copy-pasta'd from MutArrClosure:
        SmallMutArrClosure {..} -> do
            --["toMutArray", "("<>show (length mccPayload) <> " ptrs)",  intercalate "," (shorten (map (showBox 10) mccPayload))]
            payload <- traverse (showBox 10) mccPayload
            pure $ list $ (shorten payload)
        WeakClosure {} -> pure "_weak"
        IOPortClosure {} -> pure "_ioPortClosure"
        TSOClosure {} -> pure "_tsoClosure"
        StackClosure{} -> pure "_stackClosure"
      where
        showBox i' v' = self (pClos i' v')
        appBox str sel = do
            s <- showBox 10 sel
            pure $ app prec [str, s]

        shorten xs = if length xs > 20 then take 20 xs <> ["(and more)"] else xs

data HeapRoot = HeapRoot HeapGraphIndex
data HeapKV t = HeapKV HeapGraphIndex String (HeapGraphEntry t)
instance (PrintClosure r r (HeapKV t) m, MonadHeapPrint t m) => PrintClosure r Base HeapRoot m where
    pClosImpl _prec (HeapRoot r) = do
        binds <- getHeapBindings
        HeapGraph nodes <- getHeapContent
        let
            -- reachability :: IM.IntMap IS.IntSet
            -- reachability = IM.fromListWith (<>) (IM.toList (IM.map (IS.fromList . catMaybes . F.toList  . hgeClosure) nodes) <> [(k, reachable v) | (k,vs) <- IM.toList nodes, Just v <- F.toList (hgeClosure vs)])
            -- reachable :: HeapGraphIndex -> IS.IntSet
            -- reachable _ = IM.keys nodes -- IM.findWithDefault mempty x reachability
        kv <- forM  (filter (`IM.member` binds) $ IM.keys nodes) $ \k -> do
            self $ pClos 0 (HeapKV k (binds !!! k) (nodes !!! k))
        
        if IM.member r binds
        then pure $ "let" <+> align (vcat kv) <> line <> " in " <> pretty (binds !!! r)
        else pure "???"

instance (PrintClosure r r (HeapGraphEntry t) m, MonadHeapPrint t m) => PrintClosure r Base (HeapKV t) m where
  pClosImpl prec (HeapKV _ s t) = do
        p <- self $ pClos prec t
        pure (pretty s <> " = " <> p)

class Monad m => MonadDecorate m where
    decorateMap :: m (IM.IntMap Attr)
    default decorateMap :: (MonadTrans t, MonadDecorate m', m ~ t m') => m (IM.IntMap Attr)
    decorateMap = lift decorateMap
lookupDecorate :: MonadDecorate m => HeapGraphIndex -> m (Maybe Attr)
lookupDecorate i = IM.lookup i <$> decorateMap
newtype DecorateT m a = DecorateT { unDecorateT :: ReaderT (IM.IntMap Attr) m a }
    deriving newtype (Functor, Applicative, Monad, MonadTrans)
    deriving anyclass (MonadHeapPrint t)
runDecorateT :: DecorateT m a -> IM.IntMap Attr -> m a
runDecorateT (DecorateT m) = runReaderT m
instance Monad m => MonadDecorate (DecorateT m) where
    decorateMap = DecorateT ask
data DecorateElems f
instance (MonadDecorate m, PrintClosure r f (HeapKV t) m) => PrintClosure r (DecorateElems f) (HeapKV t) m where
    pClosImpl prec v@(HeapKV idx _ _) = do
        lookupDecorate idx >>= \case
          Nothing -> super (pClos prec v)
          Just dec -> annotate dec <$> super (pClos prec v)
instance (PrintClosure r x (HeapGraphIndex) m, MonadDecorate m)  => PrintClosure r (DecorateElems x) HeapGraphIndex m where
    pClosImpl prec idx =
        lookupDecorate idx >>= \case
          Nothing -> super (pClos prec idx)
          Just dec -> annotate dec <$> super (pClos prec idx)
instance {-# OVERLAPS #-}(PrintClosure r x b m, MonadDecorate m)  => PrintClosure r (DecorateElems x) b m where
    pClosImpl prec idx = super (pClos prec idx)

-- type Printer = Elide HeapData Base
type Printer = PrettyText (DecorateElems (Elide HeapData (PrettyListLiterals (NamedThunks Base))))
printClosure :: forall r f m. (PrintClosure r r f m) => Int -> f -> m (Doc Attr)
printClosure prec f = let ?printBase = proxy# @r in self (pClos prec f)

ppHeapGraph' :: IM.IntMap Attr -> HeapGraph HeapData -> Doc Attr
ppHeapGraph' attrs hg = runIdentity $ runDecorateT (runHeapPrintT (printClosure @Printer 0 (HeapRoot 0)) hg) attrs

pClos :: forall t r f x m. (?printBase :: Proxy# r, PrintClosure r x f m) => Int -> f -> Dispatch t r x (m (Doc Attr))
pClos a b = Dispatch (let ?printLocal = proxy# :: Proxy# x in pClosImpl a b)

isNil :: GenClosure b -> Bool
isNil (ConstrClosure { name = "[]", dataArgs = [], ptrArgs = []}) = True
isNil _ = False

app :: Int -> [Doc ann] -> Doc ann
app _ [a] = a  <> "()"
app prec xs = addBraces (10 <= prec) (sep xs)
addBraces :: (Monoid s, IsString s) => Bool -> s -> s
addBraces True t = "(" <> t <> ")"
addBraces False t = t

ppClosureDoc :: IM.IntMap Attr -> Int -> GenClosure HeapData -> Doc Attr
ppClosureDoc a b c = runIdentity (runDecorateT (printClosure @Printer b c) a)


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

class Monad m => MonadHeapPrint k m | m -> k where
    getHeapBindings :: m (IM.IntMap String)
    default getHeapBindings :: (m ~ t n, MonadHeapPrint k n, MonadTrans t) => m (IM.IntMap String)
    getHeapBindings = lift getHeapBindings
    getHeapContent :: m (HeapGraph k)
    default getHeapContent :: (m ~ t n, MonadHeapPrint k n, MonadTrans t) => m (HeapGraph k)
    getHeapContent = lift getHeapContent

runHeapPrintT :: HeapPrintT t m a -> HeapGraph t -> m a
runHeapPrintT (HeapPrintT r) hg = runReaderT r (hg, mkBindingMap hg)
newtype HeapPrintT t m a = HeapPrintT { unHeapPrintT :: ReaderT (HeapGraph t, IM.IntMap String) m a }
    deriving newtype (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadState s)
    deriving anyclass MonadDecorate
instance Monad m => MonadHeapPrint t (HeapPrintT t m) where
  getHeapBindings = HeapPrintT $ asks snd
  getHeapContent = HeapPrintT $ asks fst
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
    -- | In the given HeapMap, list all indices that are used more than once. The
    -- second parameter adds external references, commonly @[heapGraphRoot]@.
    bindings :: [HeapGraphIndex]
    bindings = nubOrd $ (0:) $ M.keys $  M.filter (> 1) $ M.fromListWith (+) $ map (,1) $ concatMap (catMaybes . allClosures . hgeClosure) (IM.elems m)

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
        , (v, idx) <- zip vs [1::Int ..]
        ]
    groupedBindings = M.fromListWith (<>) [ (bindingLetter i, [i]) | i <- bindings ]
    iToE i = m !!! i

(!!!) :: (HasCallStack) => IM.IntMap v -> IM.Key -> v
(!!!) m k = case IM.lookup k m of
    Nothing -> error $ "HeapGraph.!!!: key " ++ show k ++ " not found"
    Just v -> v
