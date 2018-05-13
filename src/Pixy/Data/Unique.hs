module Pixy.Data.Unique where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Data.Functor.Identity

newtype UniqueT m a = UniqueT { unUniqueT ::  (StateT Int m a) }
    deriving 
        ( Functor
        , Applicative
        , Monad
        , MonadTrans
        , MonadIO
        , MonadError e
        , MonadReader r
        )

newtype Unique a = Unique (UniqueT Identity a)
    deriving (Functor, Applicative, Monad, MonadUnique)

class Uniquable a where
    mkUnique :: Int -> a

class (Monad m) => MonadUnique m where
    fresh :: (Uniquable a) => m a

instance (Monad m) => MonadUnique (UniqueT m) where
    fresh = UniqueT $ do
        i <- get
        put (i + 1)
        return $ mkUnique i

runUniqueT :: (Monad m) => UniqueT m a -> m a
runUniqueT u = evalStateT (unUniqueT u) 0

runUnique :: Unique a -> a
runUnique (Unique u) = runIdentity $ runUniqueT u