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

class Monad m => MonadReq r m | m -> r where
    send :: r a -> m (Maybe a)
    default send :: (MonadTrans t, m ~ t n, MonadReq r n) => r a -> m (Maybe a)
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
data AsyncRequests r = AR (M.Map (Exists r) ReqState) (forall a. r a -> IO a)


-- | Add a request to the map
addRequest :: (forall x. Ord (r x), Typeable a, Typeable r) => r a -> AsyncRequests r -> AsyncRequests r
addRequest r (AR m k) = AR (M.insert (Exists r) InProgress m) k

-- | Mark a request as done
doneRequest :: (Show a, forall x. Ord (r x), Typeable a, Typeable r) => r a -> a -> AsyncRequests r -> AsyncRequests r
doneRequest r a (AR m k) = AR (M.insert (Exists r) (Done $ Boxed a) m) k

-- | Mark a request as failed
failedRequest :: (forall x. Ord (r x), Typeable a, Typeable r) => r a -> AsyncRequests r -> AsyncRequests r
failedRequest r (AR m k) = AR (M.insert (Exists r) Failed m) k

type Cache r = MVar (AsyncRequests r)
