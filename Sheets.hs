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
  , items  :: m [a]
  }

-- | Monadically traverse the rows in a table.
rows :: Monad m => ([Text] -> m r) -> Table m t -> m [r]
rows fn (Table cs ms) = ms >>= \is -> forM is $
  \i -> forM cs (($ i) . field) >>= fn

-- | Monadically traverse the columns in a table.
columns :: Monad m => ([Text] -> m c) -> Table m t -> m [c]
columns fn (Table cs ms) = ms >>= \is -> forM cs $
  \(Field c) -> forM is c >>= fn

-- | A column taking a lens into a number in the state and
-- counting up from that number.
counter :: (Show n, Num n, MonadState s m) =>
  Simple Lens s n -> Field m x
counter l = Field . const $ (l += 1)
  >> ((pack . show) `liftM` use l)

