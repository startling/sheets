module Sheets where
-- base
import Control.Applicative
import Control.Monad
import Data.List
-- mtl
import Control.Monad.State
-- Text
import Data.Text (Text)
import qualified Data.Text as T
-- lens
import Control.Lens

data Column m a = Column
  { field :: a -> m Text
  }

data Table m a = Table
  { fields :: [Column m a]
  , items  :: m [a]
  }

counter :: (Num n, Show s, MonadState s m) =>
  ASetter' s n -> Column m a
counter l = Column . const . liftM (T.pack . show) $ increment l where
  increment :: (Num n, MonadState s m) => ASetter' s n -> m s
  increment l = liftM2 const get $ l += 1

rows :: Monad m => ([Text] -> m r) -> Table m t -> m [r]
rows fn (Table cs ms) = ms >>= \is -> forM is $
  \i -> forM cs (($ i) . field) >>= fn

rows_ :: Monad m => ([Text] -> m r) -> Table m t -> m ()
rows_ fn (Table cs ms) = ms >>= \is -> forM_ is $
  \i -> forM cs (($ i) . field) >>= fn
  
columns :: Monad m => ([Text] -> m c) -> Table m t -> m [c]
columns fn (Table cs ms) = ms >>= \is -> forM cs $
  \(Column c) -> forM is c >>= fn

columns_ :: Monad m => ([Text] -> m c) -> Table m t -> m ()
columns_ fn (Table cs ms) = ms >>= \is -> forM_ cs $
  \(Column c) -> forM is c >>= fn

