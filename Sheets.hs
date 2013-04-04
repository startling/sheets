module Sheets where
-- base
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
  { columns :: [Column m a]
  , rows    :: m [a]
  }
 
counter :: (Num n, Show s, MonadState s m) =>
  ASetter' s n -> Column m a
counter l = Column . const . liftM (T.pack . show) $ increment l where
  increment :: (Num n, MonadState s m) => ASetter' s n -> m s
  increment l = liftM2 const get $ l += 1

asColumns :: Monad m => Table m t -> m [[Text]]
asColumns (Table c r) = mapM (($ r) . col) c where
  col :: Monad m => Column m a -> m [a] -> m [Text]
  col (Column a) r = r >>= mapM a

asRows :: Monad m => Table m t -> m [[Text]]
asRows = liftM transpose . asColumns
