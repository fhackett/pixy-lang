module Pixy.Pass.BufferPass where

import Control.Monad.Writer
import Control.Monad.Reader

import qualified Data.Map as Map
import Data.Map (Map, (!))

import Pixy.Data.Name
import Pixy.Data.Delay
import Pixy.Syntax

data BufferEnv = BufferEnv 
    { depth :: Int
    }

newtype BufferSizes = BufferSizes { bufferSizes :: Map Name Int }

instance Monoid BufferSizes where
    mempty = BufferSizes $ Map.empty
    mappend (BufferSizes b1) (BufferSizes b2) = BufferSizes (Map.unionWith max b1 b2)

getDepth :: (MonadReader BufferEnv m) => m Int
getDepth = asks depth

addDepth :: (MonadReader BufferEnv m) => Int -> m a -> m a
addDepth d = local (\s -> s { depth = d + (depth s) })

-- computeBuffers :: (MonadWriter BufferSizes m, MonadReader BufferEnv m) => Expr DelayPass -> m (Expr BufferPass)
-- computeBuffers = \case
--     (Var x) -> writer (Var x, BufferSizes $ Map.singleton (danName x) (getValue $ danDelay x))
--     (Const k) -> return $ Const k
--     (If c t f) -> If <$> computeBuffers c <*> computeBuffers t <*> computeBuffers f
--     -- (Fby l r) -> Fby