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

data Field m a = Field
  { field :: a -> m Text
  }

data Table m a = Table
  { fields :: [Field m a]
  , items  :: [a]
  }

-- | Monadically traverse the rows in a table.
rows :: Monad m => ([Text] -> m r) -> Table m t -> m [r]
rows fn (Table cs is) = forM is $ \i -> forM cs (($ i) . field) >>= fn

-- | Monadically traverse the columns in a table.
columns :: Monad m => ([Text] -> m c) -> Table m t -> m [c]
columns fn (Table cs is) = forM cs $ \(Field c) -> forM is c >>= fn

-- | Split a table, given the number of rows each should have.
split :: Int -> Table m a -> [Table m a]
split n (Table fs is) = Table fs `map` taking n is where
  taking :: Int -> [a] -> [[a]]
  taking n x = let (a, b) = splitAt n x in a : taking n b
  
-- | A column taking a lens into a number in the state and
-- counting up from that number.
counter :: (Show n, Num n, MonadState s m) =>
  Simple Lens s n -> Field m x
counter l = Field . const $ (l += 1)
  >> ((pack . show) `liftM` use l)

-- | A column just showing the 'Text' in the row.
see :: Monad m => Field m Text
see = Field $ return . id

data Layout a
  = Column [Either a (Layout a)]
  | Adjacent [Either a (Layout a)]
  deriving
  ( Eq
  , Ord
  , Show
  )
