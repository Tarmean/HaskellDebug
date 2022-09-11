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
import Control.Concurrent (forkFinally)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Reader

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


type Cache r = (AsyncRequests r)


newtype Caching r m a = Caching { runCaching :: ReaderT (Cache r) m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

sendRequest :: (MonadIO m, Typeable a, Typeable r, Show a, forall x. Ord (r x)) => r a -> Caching r m (Maybe a)
sendRequest r = do
    s <- Caching ask
    m <- liftIO $ readMVar (cache s)

    case M.lookup (Exists r) m of
        Just InProgress -> return Nothing
        Just (Done (Boxed a)) -> pure (cast a)
        Just Failed -> return Nothing
        Nothing -> liftIO $ do
            modifyMVar_ (cache s) $ \m -> return $ M.insert (Exists r) InProgress m
            forkFinally (process s r) $ \res -> do
                modifyMVar_ (cache s) $ \m -> return $ case res of
                    Left _ -> M.insert (Exists r) Failed m
                    Right a -> M.insert (Exists r) (Done (Boxed a)) m
                onFinish s r
            return Nothing

instance (MonadIO m) => MonadReq r (Caching r m) where
   send = sendRequest
