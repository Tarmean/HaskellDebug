{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE FunctionalDependencies #-}
module AsyncRequests where
import qualified Data.Map as M
import Data.Typeable
import Data.Kind
import Control.Monad.Trans
import Control.Concurrent.MVar
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Reader
import GHC.IO (unsafePerformIO)

class Monad m => MonadReq r m | m -> r where
    send :: (Show a, forall x. Ord (r x), Typeable r, Typeable a) => r a -> m (Maybe a)
    default send :: (Show a, forall x. Ord (r x), Typeable r, Typeable a, MonadTrans t, m ~ t n, MonadReq r n) => r a -> m (Maybe a)
    send = lift . send
instance MonadReq r m => MonadReq r (MaybeT m)

data ReqState = InProgress | Done Boxed | Failed
  deriving Show
data Exists (s :: Type -> Type) where
    Exists  :: Typeable a => s a -> Exists s
instance (Typeable f, forall x. Ord (f x)) => Eq (Exists f) where
    Exists x == Exists y = case cast y of
        Just y' -> x == y'
        Nothing -> False
instance (Typeable f, forall x. Ord (f x)) => Ord (Exists f) where
    Exists x `compare` Exists y = case cast y of
        Just y' -> x `compare` y'
        Nothing -> typeOf x `compare` typeOf y
instance (forall x. Show (f x)) => Show (Exists f) where
    show (Exists x) = show x
data Boxed where
    Boxed :: (Show a, Typeable a) => a -> Boxed
instance Show Boxed where
    show (Boxed x) = "Boxed " <> show x
data AsyncRequests r = AR { cache :: (MVar (M.Map (Exists r) (ReqState))), process ::  (forall a. r a -> IO a), onFinish :: (forall a. r a -> IO ()) }
instance (forall x. Show (r x)) => Show (AsyncRequests r) where
    show (AR mv _ _) = unsafePerformIO $ do
           o <- readMVar  mv
           pure $ show o

type Cache r = (AsyncRequests r)

newtype Caching r m a = Caching { runCaching :: ReaderT (Cache r) m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)
mkCache :: (forall x. r x -> IO x) -> (forall x. r x -> IO ()) -> IO (Cache r)
mkCache process onFinish = do
    mv <- newMVar M.empty
    pure (AR mv process onFinish)

runCachingT :: Cache r -> Caching r m a -> m a
runCachingT cache (Caching m) =
    runReaderT m cache


sendRequest :: (MonadIO m, Typeable a, Typeable r, Show a, forall x. Ord (r x)) => r a -> Caching r m (Maybe a)
sendRequest r = do
    s <- Caching ask
    m <- liftIO $ readMVar (cache s)

    case M.lookup (Exists r) m of
        Just InProgress -> return Nothing
        Just (Done (Boxed a)) -> pure (cast a)
        Just Failed -> return Nothing
        Nothing -> liftIO $ do
            modifyMVar_ (cache s) $ \m' -> return $ M.insert (Exists r) InProgress m'
            -- _tid <- forkFinally (process s r) $ \res -> do
            res <- process s r
            modifyMVar_ (cache s) $ \m' -> return $ 
                -- Left _ -> M.insert (Exists r) Failed m'
                M.insert (Exists r) (Done (Boxed res)) m'
            onFinish s r
            return (Just res)

instance (MonadIO m) => MonadReq r (Caching r m) where
   send = sendRequest
