{-# Language Rank2Types #-}
{-# Language TemplateHaskell #-}
{-# Language OverloadedStrings #-}
module Sheets
  ( Field(..)
  , classes
  , label
  , field
  , Table(..)
  , title
  , fields
  , items
  , rows
  , columns
  , transform
  , split
  , split'
  , counter
  , see
  , blank
  , (*.)
  , (!.)
  , ($.)
  , (%.)
  , Layout(..)
  , horizontal )
  where
-- mtl
import Control.Monad.State
-- Text
import Data.Text (Text)
import Data.Text (pack, empty)
-- lens
import Control.Lens hiding (transform)

data Field m a = Field
  { _classes :: [String]
  , _label :: Maybe Text
  , _field :: a -> m Text
  }
makeLenses ''Field

data Table m a = Table
  { _title  :: Maybe Text
  , _fields :: [Field m a]
  , _items  :: [a]
  }
makeLenses ''Table 

-- | Monadically traverse the rows in a table.
rows :: Monad m => ([Text] -> m r) -> Table m t -> m [r]
rows fn (Table _ cs is) = forM is $ \i -> forM cs (($ i) . view field) >>= fn

-- | Monadically traverse the columns in a table.
columns :: Monad m => ([Text] -> m c) -> Table m t -> m [c]
columns fn (Table _ cs is) = forM cs $ \(Field _ _ c) -> forM is c >>= fn

transform :: (m Text -> n Text) -> Table m a -> Table n a
transform fn (Table t f i) = Table t (map transform' f) i where
  transform' (Field c l f) = Field c l $ fn . f

-- | Split a table, given the number of rows each should have.
split :: Int -> Table m a -> [Table m a]
split n (Table t fs is) = Table t fs `map` taking n is where
  taking :: Int -> [a] -> [[a]]
  taking _ [] = []
  taking n x = let (a, b) = splitAt n x in a : taking n b

-- | Split a table into a table with twice as many fields and
-- half as many items.
split' :: Table m b -> Table m (b, b)
split' (Table t fs is) = Table t
  (map (over field (. fst)) fs ++ map (over field (. snd)) fs)
  . uncurry zip . splitAt (length is `div` 2) $ is
  
-- | A column taking a lens into a number in the state and
-- counting up from that number.
counter :: (Show n, Num n, MonadState s m) =>
  Simple Lens s n -> Field m x
counter l = Field ["count"] Nothing . const $ (l += 1)
  >> ((pack . show) `liftM` use l)

-- | A column just showing the 'Text' in the row.
see :: Monad m => Field m Text
see = Field [] Nothing $ return . id

-- | An empty column.
blank :: Monad m => Field m a
blank = Field [] Nothing $ \_ -> return empty

-- | Add a class to a 'Field'.
(*.) :: Field m a -> String -> Field m a
(*.) f s = classes <>~ [s] $ f

-- | Label a field.
(!.) :: Field m a -> Text -> Field m a
(!.) f n = label .~ Just n $ f

-- | Set the monadic function of a field.
($.) :: Field m a -> (b -> n Text) -> Field n b
($.) f g = field .~ g $ f

-- | Set the monadic function of a field from an ordinary function.
(%.) :: Monad m => Field m a -> (b -> Text) -> Field m b
(%.) f g = field .~ (return . g) $ f

data Layout a
  = Column [Either a (Layout a)]
  | Adjacent [Either a (Layout a)]
  deriving
  ( Eq
  , Ord
  , Show
  )

-- | Split a table horizontally into tables with the given
-- number of columns.
horizontal :: Int -> Table m a -> Layout (Table m a)
horizontal n = Adjacent . map Left . split n

-- | Split a table vertically into tables with the given
-- number of columns.
vertical :: Int -> Table m a -> Layout (Table m a)
vertical n = Column . map Left . split n

