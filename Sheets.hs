{-# Language Rank2Types #-}
module Sheets where
-- base
import Control.Monad
-- mtl
import Control.Monad.State
-- Text
import Data.Text (Text)
import Data.Text (pack)
-- lens
import Control.Lens

data Column m a = Column
  { field :: a -> m Text
  }

data Table m a = Table
  { fields :: [Column m a]
  , items  :: m [a]
  }

counter :: (Show n, Num n, MonadState s m) =>
  Simple Lens s n -> Column m x
counter l = Column . const $ (l += 1)
  >> ((pack . show) `liftM` use l)

rows :: Monad m => ([Text] -> m r) -> Table m t -> m [r]
rows fn (Table cs ms) = ms >>= \is -> forM is $
  \i -> forM cs (($ i) . field) >>= fn

columns :: Monad m => ([Text] -> m c) -> Table m t -> m [c]
columns fn (Table cs ms) = ms >>= \is -> forM cs $
  \(Column c) -> forM is c >>= fn
